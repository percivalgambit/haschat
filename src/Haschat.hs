module Haschat (haschat, chatServerPort, sendMessage, receiveMessage, newUser,
                userQuit, HaschatServer(..), HaschatUser(..),
                HaschatMessage) where

import Control.Concurrent (forkIO, forkFinally)
import Control.Concurrent.Async (race_)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan, dupChan)
import Control.Exception (handle)
import Control.Monad (forever, unless, void)
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

logStr :: LogLevel -> String -> IO ()
logStr level = hPrintf stderr "%s: %s\n" (show level)

-- | Chat server entry point.
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

chatServerPort :: IO Int
chatServerPort = handle handler $ do
    Just port <- readMaybe <$> getEnv "CHAT_SERVER_PORT"
    return port where
        handler e
            | isDoesNotExistError e = error "CHAT_SERVER_PORT not set."
            | isUserError e = error "cannot parse CHAT_SERVER_PORT into an integer."
            | otherwise = error "unknown error when getting CHAT_SERVER_PORT."

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

chatter :: HaschatUser -> IO ()
chatter user = do
    userHasQuit <- hIsEOF $ _userHandle user
    unless userHasQuit $ do
        messageStr <- hGetLine $ _userHandle user
        sendMessage user messageStr
        chatter user

listen :: HaschatUser -> IO ()
listen user = forever $ do
    message <- receiveMessage user
    case message of
        Just msg -> hPutStrLn (_userHandle user) msg
        Nothing  -> return ()

sendMessage :: HaschatUser -> HaschatMessage -> IO ()
sendMessage user message =
    writeChan (_userMessageChan user) $ printf "%d: %s" (_userId user) message

receiveMessage :: HaschatUser -> IO (Maybe HaschatMessage)
receiveMessage user = do
    message <- readChan $ _userMessageChan user
    let senderStr = takeWhile (/= ':') message
    if senderStr /= show (_userId user) then return $ Just message
                                        else return Nothing

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

userQuit :: HaschatUser -> IO ()
userQuit user = do
    writeChan (_userMessageChan user) $ printf "%d has left" (_userId user)
    hClose $ _userHandle user
