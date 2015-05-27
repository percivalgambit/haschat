module Main (main) where

import Test.Hspec
import Test.QuickCheck

import Haschat

import Control.Concurrent.STM.TQueue (newTQueueIO)
import Control.Concurrent.STM.TVar (newTVarIO, readTVar, writeTVar)
import Control.Monad.STM (atomically)
import Control.Exception (bracket)
import qualified Data.Set as Set
import Network (withSocketsDo, listenOn, sClose, PortID(..))
import System.Environment (setEnv, unsetEnv)
import System.IO (Handle, openTempFile, hClose)

setupMockServer :: IO HaschatServer
setupMockServer = do
    socket <- listenOn $ PortNumber 0
    queue <- newTQueueIO
    newTVarIO HaschatServerFrozen { _serverSocket       = socket
                                  , _serverNextUserId   = 1
                                  , _serverUsers        = Set.empty
                                  , _serverMessageQueue = queue
                                  }

teardownMockServer :: HaschatServer -> IO ()
teardownMockServer server = sClose =<< atomically (_serverSocket <$> readTVar server)

withMockServer :: (HaschatServer -> IO ()) -> IO ()
withMockServer = bracket setupMockServer teardownMockServer

newMockHandle :: IO Handle
newMockHandle = snd <$> openTempFile "/tmp" "haschatTest"

withMockHandle :: (Handle -> IO ()) -> IO ()
withMockHandle = bracket newMockHandle hClose

withMockUser :: HaschatServer -> (HaschatUser -> IO ()) -> IO ()
withMockUser server f = withMockHandle $ \handle -> do
    user <- addUser server handle
    atomically $ do
        frozenServer <- readTVar server
        let updatedUsers = Set.insert user $ _serverUsers frozenServer
        writeTVar server (frozenServer {_serverUsers = updatedUsers})
    f user

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

        describe "testing closures" $ do
            it "should let me instantiate objects for the tests" $
                withMockServer $ \server ->
                    withMockUser server $ \user1 -> withMockUser server $ \user2 -> do
                        users <- atomically $ _serverUsers <$> readTVar server
                        users `shouldBe` Set.fromList [user1, user2]
