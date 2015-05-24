module Haschat (haschat, defaultPort, chatServerPort) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.Environment (lookupEnv)
import System.IO (stderr, hSetBuffering, hPutStrLn, BufferMode(..), Handle)

data HaschatAction = SendMessage HaschatMessage
                   | AddUser HaschatUser
                   | RemoveUser HaschatUser

data HaschatServer = HaschatServer
    { serverSocket      :: Socket
    , serverNextUserId  :: Int
    , serverUsers       :: [HaschatUser]
    , serverHaschatChan :: Chan HaschatAction
    }

data HaschatUser = HaschatUser
    { userId          :: Int
    , userHandle      :: Handle
    , userHaschatChan :: Chan HaschatAction
    }

data HaschatMessage = HaschatMessage
    { messageBody   :: String
    , messageSender :: Maybe HaschatUser
    }

defaultPort :: Int
defaultPort = 22311

-- | Chat server entry point.
haschat :: IO ()
haschat = withSocketsDo $ do
    serverPort <- chatServerPort
    listenSock <- listenOn $ PortNumber (fromIntegral serverPort)
    hPutStrLn stderr $ "Listening on port " ++ (show serverPort)
    haschatChan <- newChan
    let server = HaschatServer { serverSocket      = listenSock
                               , serverNextUserId  = 1
                               , serverUsers       = []
                               , serverHaschatChan = haschatChan
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

serverLoop :: HaschatServer -> IO ()
serverLoop server = do
    (handle, hostname, clientPort) <- accept $ serverSocket server
    hPutStrLn stderr $ unwords
        [ "Accepted connection from"
        , hostname ++ ":" ++ show clientPort
        ]
    hSetBuffering handle NoBuffering
    let user = HaschatUser { userId    = serverNextUserId server
                           , userHandle      = handle
                           , userHaschatChan = serverHaschatChan server
                           }
    writeChan (serverHaschatChan server) $ AddUser user
    _ <- forkIO $ chatter user
    serverLoop server { serverNextUserId = serverNextUserId server + 1 }

processActions :: HaschatServer -> IO ()
processActions server = do
    action <- readChan $ serverHaschatChan server
    case action of
        SendMessage message -> processMessage server message
        AddUser user        -> processAddUser server user
        RemoveUser user     -> processRemoveUser server user
    processActions server

processMessage :: HaschatServer -> HaschatMessage -> IO ()
processMessage server message = return ()

processAddUser :: HaschatServer -> HaschatUser -> IO ()
processAddUser server user = return ()

processRemoveUser :: HaschatServer -> HaschatUser -> IO ()
processRemoveUser server user = return ()

chatter :: HaschatUser -> IO ()
chatter user = return ()
