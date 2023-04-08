{-# LANGUAGE OverloadedStrings #-}
module Controller.Index where

import Network.Wai (responseLBS, Application, getRequestBodyChunk, requestMethod, pathInfo)
import Network.HTTP.Types (status200)
import Data.ByteString.Lazy (fromStrict)
import Data.Aeson (FromJSON, ToJSON, Value(Object), parseJSON, toJSON, object, (.:), (.=), encode, decode)

data TestJson = TestJson { str :: Int, num :: Int }

instance FromJSON TestJson where
  parseJSON (Object v) =
    TestJson <$> v .: "str"
      <*> v .: "num"

instance ToJSON TestJson where
  toJSON (TestJson str num) =
    object [ "str" .= str, "num" .= num ]

main :: Application
main req respond = ioResponse >>= respond
  where
    json = getRequestBodyChunk req
    trans = responseLBS status200 []
    res = fmap (encode . transform . decode . fromStrict) json
    ioResponse = fmap trans res

transform :: Maybe TestJson -> TestJson
transform Nothing = TestJson 2 1
transform (Just test) = TestJson (str test) ((num test) + 1)

