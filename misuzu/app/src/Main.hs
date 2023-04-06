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
  putStr "start Server: http://localhost:"
  print port
  run port helloApp

helloApp :: Application
helloApp req respond = routing req respond

getPort :: IO Port
getPort = getEnvironment >>= return . port
  where
    port = fromMaybe defaultPort . fmap read . lookup "PORT"

defaultPort :: Port
defaultPort = 3000
