module Config
  ( Config (..)
  , loadConfig
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import System.Environment (lookupEnv)

data Config = Config
  { cfgDbHost :: String
  , cfgDbPort :: Int
  , cfgDbUser :: ByteString
  , cfgDbPass :: ByteString
  , cfgDbName :: ByteString
  , cfgApiKey :: ByteString
  , cfgPort :: Int
  }

loadConfig :: IO Config
loadConfig = do
  host <- envStr "DB_HOST" "127.0.0.1"
  user <- envBs "DB_USERNAME" "root"
  pass <- envBs "DB_PASSWORD" ""
  name <- envBs "DB_DATABASE" "midas"
  key <- envBs "API_KEY" ""
  port <- maybe 80 read <$> lookupEnv "PORT"
  dbPort <- maybe 3306 read <$> lookupEnv "DB_PORT"
  pure Config
    { cfgDbHost = host
    , cfgDbPort = dbPort
    , cfgDbUser = user
    , cfgDbPass = pass
    , cfgDbName = name
    , cfgApiKey = key
    , cfgPort = port
    }
  where
    envStr k d = maybe d id <$> lookupEnv k
    envBs k d = maybe d BC.pack <$> lookupEnv k
