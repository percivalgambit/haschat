module Main (main) where

import Test.Hspec
import Test.QuickCheck

import Haschat (haschat, chatServerPort, sendMessage, receiveMessage, newUser,
                userQuit, HaschatServer(..), HaschatUser(..),
                HaschatMessage)

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan (newChan)
import Control.Exception (bracket)
import Control.Monad (void, replicateM_, replicateM, foldM_)
import Data.IORef (newIORef)
import Network (withSocketsDo, listenOn, sClose, connectTo, PortID(..))
import System.Environment (setEnv, unsetEnv)
import System.IO (openTempFile, hGetLine, hPutStrLn, hClose, Handle)
import System.Random (newStdGen, randomR, StdGen)
import Text.Printf (printf)

haschatPort :: Int
haschatPort = 22311

setupMockServer :: Int -> IO HaschatServer
setupMockServer initialUserId = do
    socket <- listenOn $ PortNumber 0
    nextUserId <- newIORef initialUserId
    chan <- newChan
    return $ HaschatServer { _serverSocket      = socket
                           , _serverNextUserId  = nextUserId
                           , _serverMessageChan = chan
                           }

teardownMockServer :: HaschatServer -> IO ()
teardownMockServer = sClose . _serverSocket

withMockServer :: Int -> (HaschatServer -> IO a) -> IO a
withMockServer initialUserId = bracket (setupMockServer initialUserId)
                                       teardownMockServer

withMockUsers :: HaschatServer -> Int -> ([HaschatUser] -> IO a) -> IO a
withMockUsers server numUsers = bracket setupUsers teardownUsers where
    setupUsers = replicateM numUsers (newMockHandle >>= newUser server)
    teardownUsers users = mapM_ userQuit users
    newMockHandle = snd <$> openTempFile "/tmp" "haschatTest"

withDefaultTestHarness :: Int
                       -> (HaschatUser -> HaschatUser -> IO a)
                       -> IO a
withDefaultTestHarness initialUserId f =
    withMockServer initialUserId $ \server ->
        withMockUsers server 2 $ \[u1, u2] -> do
            f u1 u2

withHaschatServer :: IO () -> IO ()
withHaschatServer action =
    bracket (setEnv "CHAT_SERVER_PORT" (show haschatPort) >> forkIO haschat)
            killThread
            (const action)

withClientConnections :: Int -> ([Handle] -> IO a) -> IO a
withClientConnections numConnections =
    bracket (replicateM numConnections newConnection)
            (mapM_ hClose) where
        newConnection = connectTo "localhost" $ PortNumber (fromIntegral haschatPort)

receiveNextValidMessage :: HaschatUser -> IO HaschatMessage
receiveNextValidMessage user = do
    message <- receiveMessage user
    case message of
        Just msg -> return msg
        Nothing  -> receiveNextValidMessage user

discardNMessages :: Int -> HaschatUser -> IO ()
discardNMessages n = replicateM_ n . receiveMessage

main :: IO ()
main = withSocketsDo $ hspec $ do
    describe "Unit tests" $ do

        describe "the chat server port" $ do
            it "should equal CHAT_SERVER_PORT if it is set" $ property $
                \expectedPort -> do
                    setEnv "CHAT_SERVER_PORT" $ show (expectedPort :: Int)
                    port <- chatServerPort
                    port `shouldBe` expectedPort

            it "should throw an error if CHAT_SERVER_PORT is not set" $ do
                unsetEnv "CHAT_SERVER_PORT"
                void $ chatServerPort `shouldThrow` anyErrorCall

            it "should throw an error if CHAT_SERVER_PORT is not fully parsable\
               \ into an integer" $ do
                setEnv "CHAT_SERVER_PORT" "123foo"
                void $ chatServerPort `shouldThrow` anyErrorCall

        describe "a user" $ do
            it "should have a unique username determined by a counter in the\
               \ server" $ property $ \initialUserId ->
                withDefaultTestHarness initialUserId $ \u1 u2 -> do
                    _userId u1 `shouldBe` initialUserId
                    _userId u2 `shouldBe` succ initialUserId

            it "should display a message to all clients saying it has joined\
               \ the server" $ property $ \initialUserId ->
                withDefaultTestHarness initialUserId $ \u1 u2 -> do
                    let joinMessage1 = printf "%d has joined" initialUserId
                        joinMessage2 = printf "%d has joined" $ succ initialUserId

                    Just u1Message1 <- receiveMessage u1
                    Just u1Message2 <- receiveMessage u1
                    Just u2Message1 <- receiveMessage u2

                    u1Message1 `shouldBe` joinMessage1
                    u1Message2 `shouldBe` joinMessage2
                    u2Message1 `shouldBe` joinMessage2

            it "should display a message to all clients saying it has left\
               \ the server" $ property $ \initialUserId ->
                withMockServer initialUserId $ \server ->
                    withMockUsers server 1 $ \[u1] -> do
                        withMockUsers server 1 $ (void . return)

                        let leftMessage = printf "%d has left" $ succ initialUserId

                        discardNMessages 2 u1
                        Just u1Message <- receiveMessage u1
                        u1Message `shouldBe` leftMessage

            it "should be able to send messages to every other user but\
               \ itself" $ property $ \initialUserId randMsg ->
                withDefaultTestHarness initialUserId $ \u1 u2 -> do
                    discardNMessages 2 u1
                    discardNMessages 1 u2

                    sendMessage u1 randMsg
                    sendMessage u2 randMsg

                    Nothing <- receiveMessage u1
                    Just _  <- receiveMessage u1
                    Just _  <- receiveMessage u2
                    Nothing <- receiveMessage u2

                    return ()

        describe "a message" $
            it "should be prefixed with the user's id" $
                property $ \initialUserId randMsg1 randMsg2 ->
                    withDefaultTestHarness initialUserId $ \u1 u2 -> do
                        let u1SentMsg = printf "%d: %s" (_userId u1) randMsg1
                        let u2SentMsg = printf "%d: %s" (_userId u2) randMsg2

                        discardNMessages 2 u1
                        discardNMessages 1 u2
                        sendMessage u1 randMsg1
                        sendMessage u2 randMsg2

                        u1Message <- receiveNextValidMessage u1
                        u2Message <- receiveNextValidMessage u2

                        u1Message `shouldBe` u2SentMsg
                        u2Message `shouldBe` u1SentMsg

    describe "Integration tests" $ do

        describe "the entire server" $ around_ withHaschatServer $ do
            it "should be able to accept multiple incoming connections" $
                withClientConnections 2 $ \_ -> return ()

            it "should support a conversation between users" $
                --property $ \client1Messages client2Messages ->
                    withClientConnections 2 $ \[client1, client2] -> do
                        client1Join1 <- hGetLine client1
                        --client2Join1 <- hGetLine client1
                        --client2Join2 <- hGetLine client2
                        client1Join1 `shouldBe` printf joinMessageFmt (1 :: Int)
                        --client2Join1 `shouldBe` printf joinMessageFmt (2 :: Int)
                        --client2Join2 `shouldBe` printf joinMessageFmt (2 :: Int)

                        --gen <- newStdGen
                        --let messages = zip client1Messages client2Messages
                        --foldM_ (processMessages client1 client2) gen messages

                        --hClose client2
                        --client2Left1 <- hGetLine client1
                        --client2Left1 `shouldBe` printf leftMessageFmt (2 :: Int)
                        where
                            processMessages :: Handle
                                            -> Handle
                                            -> StdGen
                                            -> (String, String)
                                            -> IO StdGen
                            processMessages client1 client2 gen msg = do
                                let (clientNum, gen') = randomR (1, 2) gen
                                if (clientNum :: Int) == 1
                                then do
                                    hPutStrLn client1 $ fst msg
                                    readMsg <- hGetLine client2
                                    readMsg `shouldBe`
                                        printf messageFmt (1 :: Int) (fst msg)
                                    return gen'
                                else do
                                    hPutStrLn client2 $ snd msg
                                    readMsg <- hGetLine client1
                                    readMsg `shouldBe`
                                        printf messageFmt (2 :: Int) (snd msg)
                                    return gen'
                            joinMessageFmt = "%d has joined"
                            leftMessageFmt = "%d has left"
                            messageFmt     = "%d: %s"
