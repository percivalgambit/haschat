module Main (main) where

import Test.Hspec
import Test.QuickCheck

import Haschat

import Control.Concurrent.STM.TChan (newTChanIO)
import Control.Concurrent.STM.TVar (newTVarIO, readTVarIO)
import Control.Monad (void)
import Control.Exception (bracket)
import Network (withSocketsDo, listenOn, sClose, PortID(..))
import System.Environment (setEnv, unsetEnv)
import System.IO (Handle, openTempFile, hClose)

setupMockServer :: IO HaschatServer
setupMockServer = do
    socket <- listenOn $ PortNumber 0
    nextUserId <- newTVarIO 1
    chan <- newTChanIO
    return $HaschatServer { _serverSocket      = socket
                          , _serverNextUserId  = nextUserId
                          , _serverMessageChan = chan
                          }

teardownMockServer :: HaschatServer -> IO ()
teardownMockServer = sClose . _serverSocket

withMockServer :: (HaschatServer -> IO a) -> IO a
withMockServer = bracket setupMockServer teardownMockServer

newMockHandle :: IO Handle
newMockHandle = snd <$> openTempFile "/tmp" "haschatTest"

withMockHandle :: (Handle -> IO a) -> IO a
withMockHandle = bracket newMockHandle hClose

newMockUser :: HaschatServer -> IO HaschatUser
newMockUser = withMockHandle . newUser

withMockUser :: HaschatServer -> (HaschatUser -> IO a) -> IO a
withMockUser server = bracket (newMockUser server) (void . return)

main :: IO ()
main = withSocketsDo $ hspec $ describe "Testing haschat" $ do

        describe "the chat server port" $ do
            it "should equal CHAT_SERVER_PORT if it is set" $ property $
                \expectedPort -> do
                    setEnv "CHAT_SERVER_PORT" $ show (expectedPort :: Int)
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

        describe "testing closures" $ do
            it "should let me instantiate objects for the tests" $
                withMockServer $ \server -> withMockUser server $ \u1 ->
                    withMockUser server $ \u2 -> do
                            _userId u1 `shouldBe` 1
                            _userId u2 `shouldBe` 2
                            nextId <- readTVarIO $ _serverNextUserId server
                            nextId `shouldBe` 3
