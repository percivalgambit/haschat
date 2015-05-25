module Main (main) where

import Test.Hspec
import Test.QuickCheck

import Haschat

import Control.Concurrent.Chan (newChan)
import Control.Exception (bracket)
import Network (withSocketsDo, listenOn, sClose, PortID(..))
import System.Environment (setEnv, unsetEnv)

setupMockServer :: IO HaschatServer
setupMockServer = do
    socket <- listenOn $ PortNumber 0
    chan <- newChan
    return HaschatServer { serverSocket      = socket
                         , serverNextUserId  = 1
                         , serverUsers       = []
                         , serverHaschatChan = chan
                         }

teardownMockServer :: HaschatServer -> IO ()
teardownMockServer server = sClose $ serverSocket server

withMockServer :: (HaschatServer -> IO ()) -> IO ()
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
