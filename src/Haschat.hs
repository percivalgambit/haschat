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
    { _serverSocket    :: Socket
    , _serverNextUserId  :: Int
    , _serverUsers       :: [HaschatUser]
    , _serverHaschatChan :: Chan HaschatAction
    }

data HaschatUser = HaschatUser
    { _userId          :: Int
    , _userHandle      :: Handle
    , _userHaschatChan :: Chan HaschatAction
    }

instance Eq HaschatUser where
    u1 == u2 = _userId u1 == _userId u2

data HaschatMessage = HaschatMessage
    { _messageBody   :: String
    , _messageSender :: HaschatUser
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
    let loggerUser = HaschatUser { _userId          = 0
                                 , _userHandle      = stderr
                                 , _userHaschatChan = haschatChan
                                 }
    let server = HaschatServer { _serverSocket      = listenSock
                               , _serverNextUserId  = 1
                               , _serverUsers       = [loggerUser]
                               , _serverHaschatChan = haschatChan
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
    (handle, hostname, clientPort) <- accept $ _serverSocket server
    hPutStrLn stderr $ unwords
        [ "Accepted connection from"
        , hostname ++ ":" ++ show clientPort
        ]
    hSetBuffering handle NoBuffering
    let user = HaschatUser { _userId          = _serverNextUserId server
                           , _userHandle      = handle
                           , _userHaschatChan = _serverHaschatChan server
                           }
    writeChan (_serverHaschatChan server) $ AddUser user
    _ <- forkIO $ chatter user
    serverLoop server { _serverNextUserId = _serverNextUserId server + 1 }

sendMessage :: String -> [HaschatUser] -> IO ()
sendMessage messageStr = mapM_ (flip hPutStrLn messageStr . _userHandle)

processActions :: HaschatServer -> IO ()
processActions server = do
    action <- readChan $ _serverHaschatChan server
    updatedServer <- case action of
        (SendMessage message) -> processMessage server message
        (AddUser user) -> processAddUser server user
        (RemoveUser user) -> processRemoveUser server user
    processActions updatedServer

processMessage :: HaschatServer -> HaschatMessage -> IO HaschatServer
processMessage server message = do
    let sender = _messageSender message
        messageRecipients = filter (/= sender) (_serverUsers server)
        messageStr = addMessagePrefix sender $ _messageBody message
    sendMessage messageStr messageRecipients
    return server where
        addMessagePrefix sender messageStr =
            show (_userId sender) ++ ": " ++ messageStr


processAddUser :: HaschatServer -> HaschatUser -> IO HaschatServer
processAddUser server user = do
    let users = user : _serverUsers server
        addUserMessage = show (_userId user) ++ " has joined"
    sendMessage addUserMessage users
    return server { _serverUsers = users }

processRemoveUser :: HaschatServer -> HaschatUser -> IO HaschatServer
processRemoveUser server user = do
    let users = filter (/= user) (_serverUsers server)
        addUserMessage = show (_userId user) ++ " has left"
    sendMessage addUserMessage users
    return server { _serverUsers = users }

chatter :: HaschatUser -> IO ()
chatter user = do
    hasUserQuit <- hIsEOF $ _userHandle user
    case hasUserQuit of
        True -> writeChan (_userHaschatChan user) $ RemoveUser user
        False -> do
            messageStr <- hGetLine $ _userHandle user
            let message = HaschatMessage { _messageBody   = messageStr
                                         , _messageSender = user
                                         }
            writeChan (_userHaschatChan user) $ SendMessage message
            chatter user
