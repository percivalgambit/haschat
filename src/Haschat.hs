module Haschat (haschat, chatServerPort, sendMessage, receiveMessage, newUser,
                userQuit, HaschatServer(..), HaschatUser(..),
                HaschatMessage) where

import Control.Concurrent (forkIO, forkFinally)
import Control.Concurrent.Async (race_)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan, dupChan)
import Control.Exception (handle)
import Control.Monad (forever, unless, void)
import Data.Functor ((<$>))
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.Environment (getEnv)
import System.IO (stderr, hSetBuffering, hIsEOF, hGetLine, hPutStrLn, hClose,
                  BufferMode(..), Handle)
import System.IO.Error (isDoesNotExistError, isUserError)
import Text.Printf (printf, hPrintf)
import Text.Read (readMaybe)

data HaschatServer = HaschatServer
    { _serverSocket      :: Socket
    , _serverNextUserId  :: IORef Int
    , _serverMessageChan :: Chan HaschatMessage
    }

data HaschatUser = HaschatUser
    { _userId          :: Int
    , _userHandle      :: Handle
    , _userMessageChan :: Chan HaschatMessage
    }

type HaschatMessage = String

data LogLevel = Info
              | Warning
              | Error

instance Show LogLevel where
    show Info    = "INFO"
    show Warning = "WARNING"
    show Error   = "ERROR"

-- Log a string to stderr along with a level prefix that shows how serious the
-- logged information is
logStr :: LogLevel -> String -> IO ()
logStr level = hPrintf stderr "%s: %s\n" (show level)

-- Chat server entry point.  Runs the chat server forever if there is no error
-- with the setup.
haschat :: IO ()
haschat = withSocketsDo $ do
    serverPort <- chatServerPort
    listenSock <- listenOn $ PortNumber $ fromIntegral serverPort
    logStr Info $ printf "Listening on port %d" serverPort
    messageChan <- newChan
    nextUserId <- newIORef 0
    let server = HaschatServer { _serverSocket      = listenSock
                               , _serverNextUserId  = nextUserId
                               , _serverMessageChan = messageChan
                               }
    loggerUser <- newUser server stderr
    void $ forkIO (listen loggerUser)
    serverLoop server

-- Return the value stored in the environment variable CHAT_SERVER_PORT to be
-- used as the port number for the chat server.  If there is no value stored in
-- CHAT_SERVER_PORT or if the value cannot be parsed, then the program will exit
-- gracefully with an error message.
chatServerPort :: IO Int
chatServerPort = handle handler $ do
    Just port <- readMaybe <$> getEnv "CHAT_SERVER_PORT"
    return port where
        handler e
            | isDoesNotExistError e = error "CHAT_SERVER_PORT not set."
            | isUserError e = error "cannot parse CHAT_SERVER_PORT into an integer."
            | otherwise = error "unknown error when getting CHAT_SERVER_PORT."

-- Connection accepting loop of the server.  Will run forever, continuously
-- accepting connections, creating new user objects with the handles from the
-- connections, then forking a talking and listening thread for the user.
serverLoop :: HaschatServer -> IO ()
serverLoop server = forever $ do
    (clientHandle, hostname, clientPort) <- accept $ _serverSocket server
    logStr Info $ printf "Accepted connection from %s:%d"
                         hostname
                         (fromIntegral clientPort :: Int)
    hSetBuffering clientHandle LineBuffering
    user <- newUser server clientHandle
    void $ forkFinally (userLoop user) (\_ -> userQuit user) where
        userLoop u = race_ (chatter u) (listen u)

-- Talking loop for each user.  Will check if the user has disconnected, and if
-- not, gets a line of input from the user and broadcasts it to all other users.
chatter :: HaschatUser -> IO ()
chatter user = do
    userHasQuit <- hIsEOF $ _userHandle user
    unless userHasQuit $ do
        messageStr <- hGetLine $ _userHandle user
        sendMessage user messageStr
        chatter user

-- Listening loop for each user.  Will run forever unless killed.  Receives
-- messages from every user and displays them to the user bound to the thread.
listen :: HaschatUser -> IO ()
listen user = forever $ do
    message <- receiveMessage user
    case message of
        Just msg -> hPutStrLn (_userHandle user) msg
        Nothing  -> return ()

-- Wrapper for broadcasting a message to every user.  Prepends a prefix of the
-- sender's user id, then sends the message to every other user.
sendMessage :: HaschatUser -> HaschatMessage -> IO ()
sendMessage user message =
    writeChan (_userMessageChan user) $ printf "%d: %s" (_userId user) message

-- Wrapper for receiving messages.  Reads a message from the network, then
-- determines if it was sent from the receiver.  The message is then wrapped
-- in a Maybe monad, where it will be Just message if it was sent from a
-- different user and Nothing if it was sent from the receiver.
receiveMessage :: HaschatUser -> IO (Maybe HaschatMessage)
receiveMessage user = do
    message <- readChan $ _userMessageChan user
    let senderStr = takeWhile (/= ':') message
    if senderStr /= show (_userId user) then return $ Just message
                                        else return Nothing

-- Create a new user and update the nextUserId counter of the server.  The new
-- user will also obtain a copy of the global message channel so it can communicate
-- with other users.  A welcome message is also sent to all users (even the new one)
-- notifying them that a new user has joined.
newUser :: HaschatServer -> Handle -> IO HaschatUser
newUser server userHandle = do
    userChan <- dupChan $ _serverMessageChan server
    userId <- readIORef $ _serverNextUserId server
    modifyIORef (_serverNextUserId server) succ
    writeChan userChan $ printf "%d has joined" $ userId
    return $ HaschatUser { _userId          = userId
                         , _userHandle      = userHandle
                         , _userMessageChan = userChan
                         }

-- Perform cleanup of a user object before it dissapears.  A message will also be
-- sent to all remaining users, notifying them that this user has quit.
userQuit :: HaschatUser -> IO ()
userQuit user = do
    writeChan (_userMessageChan user) $ printf "%d has left" (_userId user)
    hClose $ _userHandle user
