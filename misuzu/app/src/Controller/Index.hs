{-# LANGUAGE OverloadedStrings #-}
module Controller.Index where

import Network.Wai (responseLBS, Application, getRequestBodyChunk, requestMethod, pathInfo)
import Network.HTTP.Types (status200)
import Data.ByteString.Lazy (fromStrict)

main :: Application
main req respond = ioResponse >>= respond
  where
    json = getRequestBodyChunk req
    trans = responseLBS status200 []
    ioResponse = fmap (trans . fromStrict) json

