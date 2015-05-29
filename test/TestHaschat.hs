module Main (main) where

import Test.Hspec
import Test.QuickCheck

import Haschat (haschat, defaultPort, chatServerPort, newUser, serverLoop,
                chatter, listen, HaschatServer(..), HaschatUser(..),
                HaschatMessage)

import Control.Concurrent.STM.TChan (newBroadcastTChanIO, tryReadTChan)
import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Exception (bracket)
import Control.Monad.STM (atomically)
import Network (withSocketsDo, listenOn, sClose, PortID(..))
import System.Environment (setEnv, unsetEnv)
import System.IO (Handle, openTempFile, hClose)
import Text.Printf (printf)

setupMockServer :: Int -> IO HaschatServer
setupMockServer initialUserId = do
    socket <- listenOn $ PortNumber 0
    nextUserId <- newTVarIO initialUserId
    chan <- newBroadcastTChanIO
    return $ HaschatServer { _serverSocket      = socket
                           , _serverNextUserId  = nextUserId
                           , _serverMessageChan = chan
                           }

teardownMockServer :: HaschatServer -> IO ()
teardownMockServer = sClose . _serverSocket

withMockServer :: Int -> (HaschatServer -> IO a) -> IO a
withMockServer initialUserId = bracket (setupMockServer initialUserId)
                                       teardownMockServer

newMockHandle :: IO Handle
newMockHandle = snd <$> openTempFile "/tmp" "haschatTest"

withMockUsers :: HaschatServer -> Int -> ([HaschatUser] -> IO a) -> IO a
withMockUsers server numUsers =
    bracket (sequence $ replicate numUsers (newMockHandle >>= newUser server))
            (\users -> sequence $ hClose . _userHandle <$> users)


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

        describe "a new user" $ do
            it "should have a unique username determined by a counter in the\
               \ server" $ property $ \initialUserId ->
                withMockServer initialUserId $ \server ->
                    withMockUsers server 2 $ \[u1, u2] -> do
                        _userId u1 `shouldBe` initialUserId
                        _userId u2 `shouldBe` succ initialUserId

            it "should display a message to all clients saying it has joined\
               \ the server" $ property $ \initialUserId ->
                withMockServer initialUserId $ \server ->
                    withMockUsers server 2 $ \[u1, u2] -> do
                        let joinMessage1 = printf "%d has joined" initialUserId
                            joinMessage2 =
                                printf "%d has joined" $ succ initialUserId

                        Just u1Message1 <- atomically $ tryReadTChan $ _userMessageChan u1
                        Just u1Message2 <- atomically $ tryReadTChan $ _userMessageChan u1
                        Just u2Message1 <- atomically $ tryReadTChan $ _userMessageChan u2

                        u1Message1 `shouldBe` joinMessage1
                        u1Message2 `shouldBe` joinMessage2
                        u2Message1 `shouldBe` joinMessage2
