module Haschat (haschat, defaultPort, chatServerPort, addUser, serverLoop,
                sendMessage, processMessages, chatter,
                HaschatServerFrozen(..), HaschatServer,
                HaschatUser(..), HaschatMessage(..)) where

import Control.Concurrent (forkIO, forkFinally)
import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, writeTQueue, readTQueue)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVarIO, readTVar,
                                    writeTVar)
import Control.Monad.STM (atomically)
import Control.Monad (forever, void, unless, join)
import qualified Data.Set as Set
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.Environment (lookupEnv)
import System.IO (stderr, hSetBuffering, hIsEOF, hGetLine, hPutStrLn, hClose,
                  BufferMode(..), Handle)
import Text.Printf (printf, hPrintf)

data HaschatServerFrozen = HaschatServerFrozen
    { _serverSocket       :: Socket
    , _serverNextUserId   :: Int
    , _serverUsers        :: Set.Set HaschatUser
    , _serverMessageQueue :: TQueue HaschatMessage
    }

instance Show HaschatServerFrozen where
    show server = printf "HaschatServerFrozen {_serverSocket = %s, \
                         \_serverNextUserId = %s, _serverUsers = %s}"
                         (show $ _serverSocket server)
                         (show $ _serverNextUserId server)
                         (show $ _serverUsers server)

type HaschatServer = TVar HaschatServerFrozen

data HaschatUser = HaschatUser
    { _userId           :: Int
    , _userHandle       :: Handle
    , _userMessageQueue :: TQueue HaschatMessage
    }

instance Eq HaschatUser where
    u1 == u2 = (_userId u1) == (_userId u2)

instance Ord HaschatUser where
    compare u1 u2 = compare (_userId u1) (_userId u2)

instance Show HaschatUser where
    show user = printf "HaschatUser {_userId = %s, _userHandle = %s}"
                       (show $ _userId user)
                       (show $ _userHandle user)

data HaschatMessage = HaschatMessage
    { _messageBody   :: String
    , _messageSender :: HaschatUser
    } deriving Show

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
    messageQueue <- newTQueueIO
    let loggerUser = HaschatUser { _userId           = 0
                                 , _userHandle       = stderr
                                 , _userMessageQueue = messageQueue
                                 }
    server <- newTVarIO $
        HaschatServerFrozen { _serverSocket       = listenSock
                            , _serverNextUserId   = 1
                            , _serverUsers        = Set.singleton loggerUser
                            , _serverMessageQueue = messageQueue
                            }
    void $ forkIO $ processMessages server
    serverLoop server

serverLoop :: HaschatServer -> IO ()
serverLoop server = forever $ do
    (handle, hostname, clientPort) <- accept . _serverSocket =<< readTVarIO server
    logStr Info $ printf "Accepted connection from %s:%d"
                         hostname
                         (fromIntegral clientPort :: Int)
    hSetBuffering handle LineBuffering
    newUser <- addUser server handle
    void $ forkFinally (chatter newUser) (\_ -> removeUser server newUser)

chatter :: HaschatUser -> IO ()
chatter user = do
    userHasQuit <- hIsEOF $ _userHandle user
    unless userHasQuit $ do
        messageStr <- hGetLine $ _userHandle user
        atomically $ do
            writeTQueue (_userMessageQueue user) $
                HaschatMessage { _messageBody   = messageStr
                               , _messageSender = user
                               }
        chatter user

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

sendMessage :: String -> Set.Set HaschatUser -> IO ()
sendMessage messageStr = mapM_ (flip hPutStrLn messageStr . _userHandle)

addUser :: HaschatServer -> Handle -> IO HaschatUser
addUser server handle = do
    (welcomeUser, newUser) <- atomically $ do
        frozenServer <- readTVar server
        let messageQueue = _serverMessageQueue frozenServer
            nextUserId   = _serverNextUserId frozenServer
            user         = HaschatUser { _userId           = nextUserId
                                       , _userHandle       = handle
                                       , _userMessageQueue = messageQueue
                                       }
            updatedUsers = Set.insert user $ _serverUsers frozenServer
        writeTVar server (frozenServer { _serverNextUserId = succ nextUserId
                                       , _serverUsers      = updatedUsers
                                       })
        welcome <- return $
            sendMessage (printf "%d has joined" $ _userId user) updatedUsers
        return (welcome, user)
    welcomeUser
    return newUser

removeUser :: HaschatServer -> HaschatUser -> IO ()
removeUser server user = do
    join $ atomically $ do
        frozenServer <- readTVar server
        let updatedUsers = Set.delete user $ _serverUsers frozenServer
        writeTVar server (frozenServer {_serverUsers = updatedUsers})
        return $ sendMessage (printf "%d has left" $ _userId user) updatedUsers
    hClose $ _userHandle user

processMessages :: HaschatServer -> IO ()
processMessages server = forever $ join $ atomically $ do
    frozenServer <- readTVar server
    message <- readTQueue $ _serverMessageQueue frozenServer
    let sender            = _messageSender message
        messageRecipients = Set.delete sender (_serverUsers frozenServer)
        messageStr        = printf "%d: %s" (_userId sender) (_messageBody message)
    return $ sendMessage messageStr messageRecipients
