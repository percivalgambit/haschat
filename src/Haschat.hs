{-# LANGUAGE PatternGuards #-}

module Haschat (haschat, defaultPort, chatServerPort, newUser, serverLoop,
                chatter, listen, HaschatServerFrozen(..), HaschatServer,
                HaschatUser(..), HaschatMessage) where

import Control.Concurrent (forkIO, forkFinally)
import Control.Concurrent.Async (race_)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, dupTChan,
                                     writeTChan, readTChan)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVarIO, readTVar,
                                    writeTVar)
import Control.Monad.STM (atomically)
import Control.Monad (forever, unless, join, void)
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.Environment (lookupEnv)
import System.IO (stderr, hSetBuffering, hIsEOF, hGetLine, hPutStrLn, hClose,
                  BufferMode(..), Handle)
import Text.Printf (printf, hPrintf)

data HaschatServerFrozen = HaschatServerFrozen
    { _serverSocket      :: Socket
    , _serverNextUserId  :: Int
    , _serverMessageChan :: TChan HaschatMessage
    }

instance Show HaschatServerFrozen where
    show server = printf "HaschatServerFrozen {_serverSocket = %s, \
                         \_serverNextUserId = %s}"
                         (show $ _serverSocket server)
                         (show $ _serverNextUserId server)

type HaschatServer = TVar HaschatServerFrozen

data HaschatUser = HaschatUser
    { _userId          :: Int
    , _userHandle      :: Handle
    , _userMessageChan :: TChan HaschatMessage
    }

instance Eq HaschatUser where
    u1 == u2 = (_userId u1) == (_userId u2)

instance Ord HaschatUser where
    compare u1 u2 = compare (_userId u1) (_userId u2)

instance Show HaschatUser where
    show user = printf "HaschatUser {_userId = %s, _userHandle = %s}"
                       (show $ _userId user)
                       (show $ _userHandle user)

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
    messageChan <- newTChanIO
    server <- newTVarIO $
        HaschatServerFrozen { _serverSocket      = listenSock
                            , _serverNextUserId  = 0
                            , _serverMessageChan = messageChan
                            }
    loggerUser <- newUser server stderr
    void $ forkIO (listen loggerUser)
    serverLoop server

serverLoop :: HaschatServer -> IO ()
serverLoop server = forever $ do
    (handle, hostname, clientPort) <- accept . _serverSocket =<< readTVarIO server
    logStr Info $ printf "Accepted connection from %s:%d"
                         hostname
                         (fromIntegral clientPort :: Int)
    hSetBuffering handle LineBuffering
    user <- newUser server handle
    forkFinally (userLoop user) (\_ -> removeUser server user)

userLoop :: HaschatUser -> IO ()
userLoop user = race_ (chatter user) (listen user)

chatter :: HaschatUser -> IO ()
chatter user = do
    userHasQuit <- hIsEOF $ _userHandle user
    unless userHasQuit $ do
        messageStr <- hGetLine $ _userHandle user
        atomically $ do
            writeTChan (_userMessageChan user) $
                printf "%d: %s" (_userId user) messageStr
        chatter user

listen :: HaschatUser -> IO ()
listen user = forever $ join $ atomically $ do
    message <- readTChan $ _userMessageChan user
    let senderStr = takeWhile (/= ':') message
    return $ unless (senderStr == show (_userId user)) $
        hPutStrLn (_userHandle user) message

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

newUser :: HaschatServer -> Handle -> IO HaschatUser
newUser server handle = atomically $ do
        frozenServer <- readTVar server
        messageChan <- dupTChan $ _serverMessageChan frozenServer
        let nextUserId   = _serverNextUserId frozenServer
            user         = HaschatUser { _userId          = nextUserId
                                       , _userHandle      = handle
                                       , _userMessageChan = messageChan
                                       }
        writeTVar server (frozenServer { _serverNextUserId = succ nextUserId
                                       })
        writeTChan messageChan $ printf "%d has joined" $ _userId user
        return user

removeUser :: HaschatServer -> HaschatUser -> IO ()
removeUser server user = do
    hClose $ _userHandle user
    atomically $ do
        messageChan <- _serverMessageChan <$> readTVar server
        writeTChan messageChan $ printf "%d has left" (_userId user)
