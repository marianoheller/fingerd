{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception
import Control.Monad (forever)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
  ( decodeUtf8,
    encodeUtf8,
  )
import Data.Typeable
import Database.SQLite.Simple hiding
  ( bind,
    close,
  )
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types
import Network.Socket
import Network.Socket.ByteString
  ( recv,
    sendAll,
  )

data User = User
  { userId :: Integer,
    username :: Text,
    shell :: Text,
    homeDirectory :: Text,
    realName :: Text,
    phone :: Text
  }
  deriving (Eq, Show)

data ProtoUser = ProtoUser
  { _username :: Text,
    _shell :: Text,
    _homeDirectory :: Text,
    _realName :: Text,
    _phone :: Text
  }
  deriving (Eq, Show)

instance FromRow User where
  fromRow =
    User <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

instance ToRow User where
  toRow (User id_ username shell homeDir realName phone) =
    toRow (id_, username, shell, homeDir, realName, phone)

instance ToRow ProtoUser where
  toRow (ProtoUser username shell homeDir realName phone) =
    toRow (Null, username, shell, homeDir, realName, phone)

allUsers :: Query
allUsers =
  "SELECT * from users"

replaceUser :: Query
replaceUser =
  "REPLACE INTO users (id, username, shell, homeDirectory, realName, phone)\
  \ VALUES (?, ?, ?, ?, ?, ?)"

type UserRow =
  (Null, Text, Text, Text, Text, Text)

getUserQuery :: Query
getUserQuery =
  "SELECT * from users where username = ?"

data DuplicateData
  = DuplicateData
  deriving (Eq, Show, Typeable)

instance Exception DuplicateData

getUser ::
  Connection ->
  Text ->
  IO (Maybe User)
getUser conn username = do
  results <- query conn getUserQuery (Only username)
  case results of
    [] -> return $ Nothing
    [user] -> return $ Just user
    _ -> throwIO DuplicateData

returnUsers ::
  Connection ->
  Socket ->
  IO ()
returnUsers dbConn soc = do
  rows <- query_ dbConn allUsers
  let usernames = map username rows
      newlineSeparated =
        T.concat $
          intersperse "\n" usernames
  sendAll soc (encodeUtf8 newlineSeparated)

formatUser :: User -> ByteString
formatUser
  ( User
      _
      username
      shell
      homeDir
      realName
      _
    ) =
    BS.concat
      [ "Login: ",
        e username,
        "\t\t\t\t",
        "Name: ",
        e realName,
        "\n",
        "Directory: ",
        e homeDir,
        "\t\t\t",
        "Shell: ",
        e shell,
        "\n"
      ]
    where
      e = encodeUtf8

insertOrUpdateUser ::
  Connection ->
  Socket ->
  ProtoUser ->
  IO ()
insertOrUpdateUser dbConn soc user = do
  execute dbConn replaceUser (toRow user)
  maybeUser <- getUser dbConn (T.strip (_username user))
  case maybeUser of
    Nothing -> do
      putStrLn "Couldn't find matching user"
    Just finalUser ->
      sendAll soc (formatUser finalUser)

parseUser :: ByteString -> Maybe ProtoUser
parseUser bs =
  case txt of
    [a, b, c, d, e] ->
      Just $
        ProtoUser a b c d e
    _ -> Nothing
  where
    txt = T.splitOn "," $ decodeUtf8 bs

handleQuery ::
  Connection ->
  Socket ->
  IO ()
handleQuery dbConn soc = do
  msg <- recv soc 1024
  case parseUser msg of
    Just protoUser ->
      insertOrUpdateUser
        dbConn
        soc
        protoUser
    _ -> returnUsers dbConn soc

handleQueries ::
  Connection ->
  Socket ->
  IO ()
handleQueries dbConn sock = forever $ do
  (soc, _) <- accept sock
  putStrLn "Got connection, handling query"
  handleQuery dbConn soc
  close soc

main :: IO ()
main = withSocketsDo $ do
  addrinfos <-
    getAddrInfo
      ( Just
          ( defaultHints
              { addrFlags = [AI_PASSIVE]
              }
          )
      )
      Nothing
      (Just "12345")
  let serveraddr = head addrinfos
  sock <-
    socket
      (addrFamily serveraddr)
      Stream
      defaultProtocol
  bind sock (addrAddress serveraddr)
  listen sock 1
  -- only one connection open at a time
  conn <- open "finger.db"
  handleQueries conn sock
  SQLite.close conn
  close sock