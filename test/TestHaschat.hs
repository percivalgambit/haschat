module Main (main) where

import Test.Hspec
import Test.QuickCheck

import Haschat

import System.Environment (setEnv, unsetEnv)

main :: IO ()
main = hspec $ describe "Testing haschat" $ do

    let expectedPort = 8081
    before_ (setEnv "CHAT_SERVER_PORT" $ show expectedPort) $ do

        describe "the chat server port" $ do
            it "should equal CHAT_SERVER_PORT if it is set" $ do
                port <- chatServerPort
                port `shouldBe` expectedPort

            it "should equal the default port if CHAT_SERVER_PORT is not set" $ do
                unsetEnv "CHAT_SERVER_PORT"
                port <- chatServerPort
                port `shouldBe` defaultPort

            it "should equal the default port if CHAT_SERVER_PORT is\
               \ not fully parsable into an integer" $ do
                setEnv "CHAT_SERVER_PORT" "123foo"
                port <- chatServerPort
                port `shouldBe` defaultPort
