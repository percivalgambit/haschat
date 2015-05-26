module Main (main) where

import Test.Hspec
import Test.QuickCheck

import Haschat

import Control.Concurrent.STM.TQueue (newTQueueIO)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar)
import Control.Monad.STM (atomically)
import Control.Exception (bracket)
import Network (withSocketsDo, listenOn, sClose, PortID(..))
import System.Environment (setEnv, unsetEnv)

setupMockServer :: IO (TVar HaschatServer)
setupMockServer = do
    socket <- listenOn $ PortNumber 0
    queue <- newTQueueIO
    newTVarIO HaschatServer { _serverSocket       = socket
                            , _serverNextUserId   = 1
                            , _serverUsers        = []
                            , _serverHaschatQueue = queue
                            }

teardownMockServer :: TVar HaschatServer -> IO ()
teardownMockServer server = sClose =<< atomically (_serverSocket <$> readTVar server)

withMockServer :: (TVar HaschatServer -> IO ()) -> IO ()
withMockServer = bracket setupMockServer teardownMockServer

main :: IO ()
main = withSocketsDo $ hspec $ describe "Testing haschat" $ do

        describe "the chat server port" $ do
            it "should equal CHAT_SERVER_PORT if it is set" $ do
                let expectedPort = 8081
                setEnv "CHAT_SERVER_PORT" $ show expectedPort
                port <- chatServerPort
                port `shouldBe` expectedPort

            it "should equal the default port if CHAT_SERVER_PORT is not set" $ do
                unsetEnv "CHAT_SERVER_PORT"
                port <- chatServerPort
                port `shouldBe` defaultPort

            it "should equal the default port if CHAT_SERVER_PORT is not fully\
               \ parsable into an integer" $ do
                setEnv "CHAT_SERVER_PORT" "123foo"
                port <- chatServerPort
                port `shouldBe` defaultPort
