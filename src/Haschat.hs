module Haschat (haschat, defaultPort, chatServerPort, newUser, serverLoop,
                chatter, listen, HaschatServer(..), HaschatUser(..),
                HaschatMessage) where

import Control.Concurrent (forkIO, forkFinally)
import Control.Concurrent.Async (race_)
import Control.Concurrent.STM.TChan (TChan, newBroadcastTChanIO, dupTChan,
                                     writeTChan, readTChan)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Monad.STM (atomically)
import Control.Monad (forever, unless, join, void)
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.Environment (lookupEnv)
import System.IO (stderr, hSetBuffering, hIsEOF, hGetLine, hPutStrLn, hClose,
                  BufferMode(..), Handle)
import Text.Printf (printf, hPrintf)

data HaschatServer = HaschatServer
    { _serverSocket      :: Socket
    , _serverNextUserId  :: TVar Int
    , _serverMessageChan :: TChan HaschatMessage
    }

data HaschatUser = HaschatUser
    { _userId          :: Int
    , _userHandle      :: Handle
    , _userMessageChan :: TChan HaschatMessage
    }

type HaschatMessage = String

data LogLevel = Info
              | Warning
              | Error

instance Show LogLevel where
    show Info    = "INFO"
    show Warning = "WARNING"
    show Error   = "ERROR"

defaultPort :: Int
defaultPort = 22311

logStr :: LogLevel -> String -> IO ()
logStr level = hPrintf stderr "%s: %s\n" (show level)

-- | Chat server entry point.
haschat :: IO ()
haschat = withSocketsDo $ do
    serverPort <- chatServerPort
    listenSock <- listenOn $ PortNumber $ fromIntegral serverPort
    logStr Info $ printf "Listening on port %d" serverPort
    messageChan <- newBroadcastTChanIO
    nextUserId <- newTVarIO 0
    let server = HaschatServer { _serverSocket      = listenSock
                               , _serverNextUserId  = nextUserId
                               , _serverMessageChan = messageChan
                               }
    loggerUser <- newUser server stderr
    void $ forkIO (listen loggerUser)
    serverLoop server

chatServerPort :: IO Int
chatServerPort = do
    maybePort <- lookupEnv "CHAT_SERVER_PORT"
    case maybePort of
        Just portStr -> case reads portStr of
            [(portNum, "")] -> return $ portNum
            _ -> do
                logStr Warning $
                    printf "cannot parse CHAT_SERVER_PORT into an integer.\
                           \ Using default port %d" defaultPort
                return defaultPort
        Nothing -> do
            logStr Warning $ printf "CHAT_SERVER_PORT not set.\
                                    \ Using default port %d" defaultPort
            return defaultPort

serverLoop :: HaschatServer -> IO ()
serverLoop server = forever $ do
    (handle, hostname, clientPort) <- accept $ _serverSocket server
    logStr Info $ printf "Accepted connection from %s:%d"
                         hostname
                         (fromIntegral clientPort :: Int)
    hSetBuffering handle LineBuffering
    user <- newUser server handle
    forkFinally (userLoop user) (\_ -> userQuit user)

userLoop :: HaschatUser -> IO ()
userLoop user = race_ (chatter user) (listen user)

chatter :: HaschatUser -> IO ()
chatter user = do
    userHasQuit <- hIsEOF $ _userHandle user
    unless userHasQuit $ do
        messageStr <- hGetLine $ _userHandle user
        atomically $
            writeTChan (_userMessageChan user) $
                printf "%d: %s" (_userId user) messageStr
        chatter user

listen :: HaschatUser -> IO ()
listen user = forever $ join $ atomically $ do
    message <- readTChan $ _userMessageChan user
    let senderStr = takeWhile (/= ':') message
    return $ unless (senderStr == show (_userId user)) $
        hPutStrLn (_userHandle user) message

newUser :: HaschatServer -> Handle -> IO HaschatUser
newUser server handle = atomically $ do
    userChan <- dupTChan $ _serverMessageChan server
    userId <- readTVar $ _serverNextUserId server
    writeTVar (_serverNextUserId server) (succ userId)
    let user = HaschatUser { _userId          = userId
                           , _userHandle      = handle
                           , _userMessageChan = userChan
                           }
    writeTChan userChan $ printf "%d has joined" $ _userId user
    return user

userQuit :: HaschatUser -> IO ()
userQuit user = do
    hClose $ _userHandle user
    atomically $
        writeTChan (_userMessageChan user) $ printf "%d has left" (_userId user)
