{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run, Port)
import System.Environment (getEnvironment)
import Data.Maybe
import Routing (routing)

main :: IO ()
main = do
  port <- getPort
  run port routing
  where
    getPort = getEnvironment >>= return . port
    port = fromMaybe 3000 . fmap read . lookup "PORT"
