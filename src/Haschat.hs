module Haschat (haschat, defaultPort, chatServerPort) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.Environment (lookupEnv)
import System.IO (stderr, hSetBuffering, hPutStrLn, BufferMode(..), Handle)

data User = User { userId       :: Int
                 , userHandle   :: Handle
                 , userMessageQueue :: Chan Message
                 }

data Message = Message { messageFrom :: User
                       , messageBody :: String
                       , doBroadcast :: Bool
                       }

data HaschatServer = HaschatServer { serverSocket :: Socket
                                   , nextUserId   :: Int
                                   , messageQueue :: Chan Message
                                   , serverUsers  :: MVar [User]
                                   }

defaultPort :: Int
defaultPort = 22311

-- | Chat server entry point.
haschat :: IO ()
haschat = withSocketsDo $ do
    serverPort <- chatServerPort
    listenSock <- listenOn $ PortNumber (fromIntegral serverPort)
    hPutStrLn stderr $ "Listening on port " ++ (show serverPort)
    messageChan <- newChan
    userList <- newMVar []
    let server = HaschatServer { serverSocket = listenSock
                               , nextUserId   = 1
                               , messageQueue = messageChan
                               , serverUsers        = userList
                               }
    _ <- forkIO $ processChat server
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

serverLoop :: HaschatServer -> IO ()
serverLoop server = do
    (handle, hostname, clientPort) <- accept $ serverSocket server
    hPutStrLn stderr $ unwords
        [ "Accepted connection from"
        , hostname ++ ":" ++ show clientPort
        ]
    hSetBuffering handle NoBuffering
    let user = User { userId           = nextUserId server
                    , userHandle       = handle
                    , userMessageQueue = messageQueue server
                    }
    addUser (serverUsers server) user
    let userJoinedMessage = Message { messageFrom = user
                                    , messageBody = show (userId user) ++ " has joined"
                                    , doBroadcast = True
                                    }
    writeChan (messageQueue server) userJoinedMessage
    _ <- forkIO $ chatter user
    serverLoop server {nextUserId = nextUserId server + 1}

addUser :: MVar [User] -> User -> IO ()
addUser users u =  putMVar users =<< (:) u <$> takeMVar users

processChat :: HaschatServer -> IO ()
processChat server = do
    nextMessage <- readChan $ messageQueue server
    userList <- takeMVar $ serverUsers server
    if doBroadcast nextMessage then broadcastMessage userList nextMessage
                               else sendMessage userList nextMessage
    putMVar (serverUsers server) userList
    processChat server where
        broadcastMessage userList message =
            mapM_ (flip hPutStrLn (messageBody message) . userHandle) userList
        sendMessage userList message =
            broadcastMessage (filter (notSender message) userList) message
        notSender message user = not $ userId user == userId (messageFrom message)

chatter :: User -> IO ()
chatter user = return ()
