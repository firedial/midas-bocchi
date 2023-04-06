{-# LANGUAGE OverloadedStrings #-}
module Routing where

import Network.Wai (responseLBS, Application, getRequestBodyChunk, requestMethod, pathInfo)
import Network.HTTP.Types (status200)
import Controller.Index

routing :: Application
routing req respond
  | method == "POST" && head path == "test1" = Controller.Index.main req respond
  | otherwise = respond $ responseLBS status200 [] "hello!"
  where
    method = requestMethod req
    path = pathInfo req
