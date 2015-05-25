module Haschat (haschat, defaultPort, chatServerPort, serverLoop, sendMessage,
                processActions, chatter, HaschatAction(..), HaschatServer(..),
                HaschatUser(..), HaschatMessage(..)) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.Environment (lookupEnv)
import System.IO (stderr, hSetBuffering, hPutStrLn, hIsEOF, hGetLine,
                  BufferMode(..), Handle)

data HaschatAction = SendMessage HaschatMessage
                   | AddUser HaschatUser
                   | RemoveUser HaschatUser

data HaschatServer = HaschatServer
    { serverSocket      :: Socket
    , serverNextUserId  :: Int
    , serverUsers       :: [HaschatUser]
    , serverHaschatChan :: Chan HaschatAction
    }

data HaschatUser = HaschatUser
    { userId          :: Int
    , userHandle      :: Handle
    , userHaschatChan :: Chan HaschatAction
    }

instance Eq HaschatUser where
    u1 == u2 = userId u1 == userId u2

data HaschatMessage = HaschatMessage
    { messageBody   :: String
    , messageSender :: HaschatUser
    }

defaultPort :: Int
defaultPort = 22311

-- | Chat server entry point.
haschat :: IO ()
haschat = withSocketsDo $ do
    serverPort <- chatServerPort
    listenSock <- listenOn $ PortNumber (fromIntegral serverPort)
    hPutStrLn stderr $ "Listening on port " ++ (show serverPort)
    haschatChan <- newChan
    let stderrUser = HaschatUser { userId          = 0
                                 , userHandle      = stderr
                                 , userHaschatChan = haschatChan
                                 }
    let server = HaschatServer { serverSocket      = listenSock
                               , serverNextUserId  = 1
                               , serverUsers       = [stderrUser]
                               , serverHaschatChan = haschatChan
                               }
    _ <- forkIO $ processActions server
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
    let user = HaschatUser { userId    = serverNextUserId server
                           , userHandle      = handle
                           , userHaschatChan = serverHaschatChan server
                           }
    writeChan (serverHaschatChan server) $ AddUser user
    _ <- forkIO $ chatter user
    serverLoop server { serverNextUserId = serverNextUserId server + 1 }

sendMessage :: String -> [HaschatUser] -> IO ()
sendMessage messageStr = mapM_ (flip hPutStrLn messageStr . userHandle)

processActions :: HaschatServer -> IO ()
processActions server = do
    action <- readChan $ serverHaschatChan server
    updatedServer <- case action of
        (SendMessage message) -> processMessage server message
        (AddUser user) -> processAddUser server user
        (RemoveUser user) -> processRemoveUser server user
    processActions updatedServer

processMessage :: HaschatServer -> HaschatMessage -> IO HaschatServer
processMessage server message = do
    let sender = messageSender message
        messageRecipients = filter (/= sender) (serverUsers server)
        messageStr = addMessagePrefix sender $ messageBody message
    sendMessage messageStr messageRecipients
    return server where
        addMessagePrefix sender messageStr =
            show (userId sender) ++ ": " ++ messageStr


processAddUser :: HaschatServer -> HaschatUser -> IO HaschatServer
processAddUser server user = do
    let users = user : serverUsers server
        addUserMessage = show (userId user) ++ " has joined"
    sendMessage addUserMessage users
    return server { serverUsers = users }

processRemoveUser :: HaschatServer -> HaschatUser -> IO HaschatServer
processRemoveUser server user = do
    let users = filter (/= user) (serverUsers server)
        addUserMessage = show (userId user) ++ " has left"
    sendMessage addUserMessage users
    return server { serverUsers = users }

chatter :: HaschatUser -> IO ()
chatter user = do
    hasUserQuit <- hIsEOF $ userHandle user
    case hasUserQuit of
        True -> writeChan (userHaschatChan user) $ RemoveUser user
        False -> do
            messageStr <- hGetLine $ userHandle user
            let message = HaschatMessage { messageBody   = messageStr
                                         , messageSender = user
                                         }
            writeChan (userHaschatChan user) $ SendMessage message
            chatter user
