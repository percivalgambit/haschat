module Haschat (haschat, defaultPort, chatServerPort) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan)
import Control.Concurrent.MVar (MVar)
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.Environment (lookupEnv)
import System.IO (stderr, hSetBuffering, hPutStrLn, BufferMode(..))

data HaschatServer = HaschatServer { serverSocket :: Socket
                                   , nextUserId   :: Int
                                   , messageQueue :: Chan String
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
    _ <- forkIO $ processChat messageChan
    let server = HaschatServer { serverSocket = listenSock
                               , nextUserId   = 1
                               , messageQueue = messageChan
                               }
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
    _ <- forkIO $ chatter server
    serverLoop server {nextUserId = nextUserId server + 1}

processChat :: Chan String -> IO ()
processChat messageChan = return ()

chatter :: HaschatServer -> IO ()
chatter server = return ()
