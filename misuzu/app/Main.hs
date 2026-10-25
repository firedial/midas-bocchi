-- | 配線 (DI) だけを行う層。ここが唯一「全部を知っている」場所。
module Main (main) where

import Network.Wai.Handler.Warp (run)

import Adapter.Http.Server (Repositories (..), application)
import Adapter.Persistence.AttributeCategoryRepository (newAttributeCategoryRepository)
import Adapter.Persistence.AttributeElementRepository (newAttributeElementRepository)
import Adapter.Persistence.BalanceRepository (newBalanceRepository)
import Adapter.Persistence.Db (DbConnectInfo (..), newDb)
import Adapter.Persistence.MoveRepository (newMoveRepository)
import Adapter.Persistence.TemplateRepository (newTemplateRepository)
import Config (Config (..), loadConfig)

main :: IO ()
main = do
  cfg <- loadConfig
  db <- newDb DbConnectInfo
    { dbHost = cfgDbHost cfg
    , dbPort = cfgDbPort cfg
    , dbUser = cfgDbUser cfg
    , dbPass = cfgDbPass cfg
    , dbName = cfgDbName cfg
    }
  let repos = Repositories
        { balanceRepo = newBalanceRepository db
        , moveRepo = newMoveRepository db
        , elementRepo = newAttributeElementRepository db
        , categoryRepo = newAttributeCategoryRepository db
        , templateRepo = newTemplateRepository db
        }
  putStrLn ("misuzu listening on port " <> show (cfgPort cfg))
  run (cfgPort cfg) (application (cfgApiKey cfg) repos)
