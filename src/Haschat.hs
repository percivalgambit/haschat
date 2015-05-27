module Haschat (haschat, defaultPort, chatServerPort, newUser, serverLoop,
                sendMessage, processActions, chatter,
                HaschatServerFrozen(..), HaschatServer,
                HaschatUser(..), HaschatMessage(..)) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, writeTQueue, readTQueue)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVarIO, readTVar,
                                    writeTVar)
import Control.Monad.STM (atomically)
import Control.Monad (forever, void)
import qualified Data.Set as Set
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.Environment (lookupEnv)
import System.IO (stderr, hSetBuffering, hPutStrLn, hIsEOF, hGetLine,
                  BufferMode(..), Handle)

data HaschatAction = AddUser Socket
                   | RemoveUser HaschatUser
                   | SendMessage HaschatMessage

data HaschatServerFrozen = HaschatServerFrozen
    { _serverSocket      :: Socket
    , _serverNextUserId  :: Int
    , _serverUsers       :: Set.Set HaschatUser
    , _serverActionQueue :: TQueue HaschatAction
    }

instance Show HaschatServerFrozen where
    show server = "HaschatServerFrozen {"
                  ++ show (_serverSocket server)     ++ ", "
                  ++ show (_serverNextUserId server) ++ ", "
                  ++ show (_serverUsers server)      ++ "}"

type HaschatServer = TVar HaschatServerFrozen

data HaschatUser = HaschatUser
    { _userId          :: Int
    , _userHandle      :: Handle
    , _userActionQueue :: TQueue HaschatAction
    }

instance Ord HaschatUser where
    compare u1 u2 = compare (_userId u1) (_userId u2)

instance Show HaschatUser where
    show user = "HaschatUser {"
                ++ show (_userId user)     ++ ", "
                ++ show (_userHandle user) ++ "}"

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
    logStr $ "Listening on port " ++ (show serverPort)
    actionQueue <- newTQueueIO
    let loggerUser = HaschatUser { _userId           = 0
                                 , _userHandle       = stderr
                                 , _userActionQueue = actionQueue
                                 }
    server <- newTVarIO $
        HaschatServerFrozen { _serverSocket       = listenSock
                            , _serverNextUserId   = 1
                            , _serverUsers        = Set.singleton loggerUser
                            , _serverActionQueue = actionQueue
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
                logStr $ "WARNING: cannot parse CHAT_SERVER_PORT into an integer."
                      ++ " Using default port " ++ show defaultPort
                return defaultPort
        Nothing -> do
            logStr $ "WARNING: CHAT_SERVER_PORT not set."
                  ++ " Using default port " ++ show defaultPort
            return defaultPort

logStr :: String -> IO ()
logStr = hPutStrLn stderr

newUser :: HaschatServer -> Handle -> IO HaschatUser
newUser server handle = atomically $ do
    frozenServer <- readTVar server
    let actionQueue = _serverActionQueue frozenServer
        nextUserId   = _serverNextUserId frozenServer
        user = HaschatUser { _userId           = nextUserId
                           , _userHandle       = handle
                           , _userActionQueue = actionQueue
                           }
        updatedUsers = Set.insert user $ _serverUsers frozenServer
    writeTVar server (frozenServer { _serverNextUserId = succ nextUserId
                                   , _serverUsers = updatedUsers
                                   })
    sendMessage (_userId user ++ " has joined") updatedUsers
    return user

removeUser :: HaschatServer -> HaschatUser -> IO ()
removeUser server user = atomically $ do
    frozenServer <- readTVar server
    let updatedUsers = Set.delete user $ _serverUsers frozenServer
    writeTVar server (frozenServer {_serverUsers = updatedUsers})
    sendMessage (_userId user ++ " has left") updatedUsers

serverLoop :: HaschatServer -> IO ()
serverLoop server = forever $ do
    (handle, hostname, clientPort) <- accept . _serverSocket =<< readTVarIO server
    logStr $ "Accepted connection from"
          ++ hostname ++ ":" ++ show clientPort
    hSetBuffering handle LineBuffering
    atomically $ do
        actionQueue <- _serverActionQueue <$> readTVar server
        writeTQueue actionQueue $ AddUser handle

sendMessage :: String -> Set.Set HaschatUser -> IO ()
sendMessage messageStr = mapM_ (flip hPutStrLn messageStr . _userHandle)

sendMessage' :: HaschatServer -> HaschatMessage -> IO ()
sendMessage' server message = do
    frozenServer <- readTVarIO server
    let sender            = _messageSender message
        messageRecipients = Set.delete sender (_serverUsers server)
        messageStr        = show (_userId sender) ++ ": " ++ _messageBody message
    sendMessage messageStr messageRecipients

processActions :: HaschatServer -> IO ()
processActions server = forever $ do
    action <- atomically $ do
        haschatQueue <- _serverHaschatQueue <$> readTVar server
        readTQueue haschatQueue
    case action of
        (SendMessage message) -> sendMessage' server message
        (AddUser handle) -> void $ newUser server handle
        (RemoveUser user) -> removeUser server user

chatter :: HaschatUser -> IO ()
chatter user = do
    userHasQuit <- hIsEOF $ _userHandle user
    case userHasQuit of
        True -> atomically $ writeTQueue (_userActionQueue user) $ RemoveUser user
        False -> do
            messageStr <- hGetLine $ _userHandle user
            atomically $ do
                writeTQueue (_userActionQueue user) $ SendMessage $
                    HaschatMessage { _messageBody   = messageStr
                                   , _messageSender = user
                                   }
            chatter user
