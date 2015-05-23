{-# LANGUAGE PatternGuards #-}

module Haschat (haschat, defaultPort, chatServerPort) where

import Control.Concurrent (forkIO)
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.Environment (lookupEnv)
import System.IO (stderr, hSetBuffering, hPutStrLn, BufferMode(..), Handle)

defaultPort :: Int
defaultPort = 22311

-- | Chat server entry point.
haschat :: IO ()
haschat = withSocketsDo $ do
    serverPort <- chatServerPort
    listenSock <- listenOn $ PortNumber (fromIntegral serverPort)
    hPutStrLn stderr $ "Listening on port " ++ (show serverPort)
    serverLoop listenSock 1

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

serverLoop :: Socket -> Int -> IO ()
serverLoop listenSock userId = do
    (handle, hostname, clientPort) <- accept listenSock
    hPutStrLn stderr $ unwords
        [ "Accepted connection from"
        , hostname ++ ":" ++ (show clientPort)
        ]
    hSetBuffering handle NoBuffering
    _ <- forkIO $ processChat handle userId
    serverLoop listenSock (userId + 1)

processChat :: Handle -> Int -> IO ()
processChat handle userId = return ()
