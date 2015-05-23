{-# LANGUAGE PatternGuards #-}

module Haschat (haschat, defaultPort, chatServerPort) where

import Control.Concurrent (forkIO)
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.Environment (lookupEnv)
import System.IO (stderr, hSetBuffering, hPutStrLn, BufferMode(..))

defaultPort :: Int
defaultPort = 22311

-- | Chat server entry point.
haschat :: IO ()
haschat = return ()

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
