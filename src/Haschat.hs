module Haschat (haschat, defaultPort, chatServerPort, serverLoop, sendMessage,
                processActions, chatter, HaschatAction(..), HaschatServer(..),
                HaschatUser(..), HaschatMessage(..)) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, writeTQueue, readTQueue)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVarIO, readTVar,
                                    writeTVar)
import Control.Monad.STM (atomically)
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.Environment (lookupEnv)
import System.IO (stderr, hSetBuffering, hPutStrLn, hIsEOF, hGetLine,
                  BufferMode(..), Handle)

data HaschatAction = SendMessage HaschatMessage
                   | AddUser HaschatUser
                   | RemoveUser HaschatUser

data HaschatServer = HaschatServer
    { _serverSocket       :: Socket
    , _serverNextUserId   :: Int
    , _serverUsers        :: [HaschatUser]
    , _serverHaschatQueue :: TQueue HaschatAction
    }

data HaschatUser = HaschatUser
    { _userId           :: Int
    , _userHandle       :: Handle
    , _userHaschatQueue :: TQueue HaschatAction
    }

instance Eq HaschatUser where
    u1 == u2 = _userId u1 == _userId u2

data HaschatMessage = HaschatMessage
    { _messageBody   :: String
    , _messageSender :: HaschatUser
    }

defaultPort :: Int
defaultPort = 22311

-- | Chat server entry point.
haschat :: IO ()
haschat = withSocketsDo $ do
    serverPort <- chatServerPort
    listenSock <- listenOn $ PortNumber (fromIntegral serverPort)
    hPutStrLn stderr $ "Listening on port " ++ (show serverPort)
    haschatQueue <- newTQueueIO
    let loggerUser = HaschatUser { _userId           = 0
                                 , _userHandle       = stderr
                                 , _userHaschatQueue = haschatQueue
                                 }
    server <- newTVarIO $
        HaschatServer { _serverSocket       = listenSock
                      , _serverNextUserId   = 1
                      , _serverUsers        = [loggerUser]
                      , _serverHaschatQueue = haschatQueue
                      }
    _ <- forkIO $ processActions server
    serverLoop server

chatServerPort :: IO Int
chatServerPort = do
    maybePort <- lookupEnv "CHAT_SERVER_PORT"
    case maybePort of
        Just portStr -> case reads portStr of
            [(portNum, "")] -> return portNum
            _ -> do
                hPutStrLn stderr $ unwords
                    [ "WARNING: cannot parse CHAT_SERVER_PORT into an integer."
                    , "Using default port " ++ show defaultPort
                    ]
                return defaultPort
        Nothing -> do
            hPutStrLn stderr $ unwords
                [ "WARNING: CHAT_SERVER_PORT not set."
                , "Using default port " ++ show defaultPort
                ]
            return defaultPort

serverLoop :: TVar HaschatServer -> IO ()
serverLoop server = do
    (handle, hostname, clientPort) <- accept =<< _serverSocket <$> readTVarIO server
    hPutStrLn stderr $ unwords
        [ "Accepted connection from"
        , hostname ++ ":" ++ show clientPort
        ]
    hSetBuffering handle LineBuffering
    atomically $ do
        frozenServer <- readTVar server
        let haschatQueue = _serverHaschatQueue frozenServer
            nextUserId   = _serverNextUserId frozenServer
        writeTQueue haschatQueue $ AddUser $
            HaschatUser { _userId           = nextUserId
                        , _userHandle       = handle
                        , _userHaschatQueue = haschatQueue
                        }
        writeTVar server (frozenServer {_serverNextUserId = succ nextUserId})
    serverLoop server

sendMessage :: String -> [HaschatUser] -> IO ()
sendMessage messageStr = mapM_ (flip hPutStrLn messageStr . _userHandle)

processActions :: TVar HaschatServer -> IO ()
processActions server = do
    action <- atomically $ do
        haschatQueue <- _serverHaschatQueue <$> readTVar server
        readTQueue haschatQueue
    case action of
        (SendMessage message) -> processMessage server message
        (AddUser user) -> processAddUser server user
        (RemoveUser user) -> processRemoveUser server user
    processActions server

processMessage :: TVar HaschatServer -> HaschatMessage -> IO ()
processMessage server message = do
    let sender = _messageSender message
        messageStr = addMessagePrefix sender $ _messageBody message
    messageRecipients <- atomically $
        filter (/= sender) <$> _serverUsers <$> readTVar server
    sendMessage messageStr messageRecipients where
        addMessagePrefix sender messageStr =
            show (_userId sender) ++ ": " ++ messageStr


processAddUser :: TVar HaschatServer -> HaschatUser -> IO ()
processAddUser server user = do
    let addUserMessage = show (_userId user) ++ " has joined"
    users <- atomically $ do
        frozenServer <- readTVar server
        let updatedUsers = user : (_serverUsers frozenServer)
        writeTVar server (frozenServer {_serverUsers = updatedUsers})
        return updatedUsers
    sendMessage addUserMessage users
    _ <- forkIO $ chatter user
    return ()

processRemoveUser :: TVar HaschatServer -> HaschatUser -> IO ()
processRemoveUser server user = do
    let removeUserMessage = show (_userId user) ++ " has left"
    users <- atomically $ do
        frozenServer <- readTVar server
        let updatedUsers = filter (/= user) $ _serverUsers frozenServer
        writeTVar server (frozenServer {_serverUsers = updatedUsers})
        return updatedUsers
    sendMessage removeUserMessage users

chatter :: HaschatUser -> IO ()
chatter user = do
    hasUserQuit <- hIsEOF $ _userHandle user
    case hasUserQuit of
        True -> atomically $ writeTQueue (_userHaschatQueue user) $ RemoveUser user
        False -> do
            messageStr <- hGetLine $ _userHandle user
            atomically $ do
                writeTQueue (_userHaschatQueue user) $ SendMessage $
                    HaschatMessage { _messageBody   = messageStr
                                   , _messageSender = user
                                   }
            chatter user
