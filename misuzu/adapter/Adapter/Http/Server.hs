-- | Servant のルーティングと HTTP 境界。リポジトリ (ポート実装) を注入され、
-- 各ハンドラを組み立てる。認証・フォールバック・エラー整形もここ。
module Adapter.Http.Server
  ( Repositories (..)
  , application
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Tagged (Tagged (..))
import qualified Data.Text as T

import Network.HTTP.Types (hAuthorization)
import Network.Wai (Application, Middleware, requestHeaders)
import Servant

import Adapter.Http.Api (API, api)
import Adapter.Http.Error (AppM, jsonResponse, toServerError)
import qualified Adapter.Http.AttributeCategory as Category
import qualified Adapter.Http.AttributeElement as Element
import qualified Adapter.Http.Balance as Balance
import qualified Adapter.Http.Move as Move
import qualified Adapter.Http.Template as Template
import Domain.Error (AppError (..), ErrorCode (..))
import Domain.Repository.AttributeCategory (AttributeCategoryRepository)
import Domain.Repository.AttributeElement (AttributeElementRepository)
import Domain.Repository.Balance (BalanceRepository)
import Domain.Repository.Move (MoveRepository)
import Domain.Repository.Template (TemplateRepository)

-- | 注入されるポート実装一式。配線 (DI) は app/Main が行う。
data Repositories = Repositories
  { balanceRepo :: BalanceRepository IO
  , moveRepo :: MoveRepository IO
  , elementRepo :: AttributeElementRepository IO
  , categoryRepo :: AttributeCategoryRepository IO
  , templateRepo :: TemplateRepository IO
  }

server :: Repositories -> Server API
server repos =
       balancesServer
  :<|> movesServer
  :<|> elementsServer
  :<|> categoriesServer
  :<|> templatesServer
  :<|> fallback
  where
    balancesServer =
           (\l o -> runApp (Balance.indexBalance (balanceRepo repos) l o))
      :<|> (\b -> runApp (Balance.storeBalance (balanceRepo repos) b))
      :<|> (\i -> runApp (Balance.showBalance (balanceRepo repos) i))
      :<|> (\i b -> runApp (Balance.updateBalance (balanceRepo repos) i b))
      :<|> (\i -> runApp (Balance.destroyBalance (balanceRepo repos) i))
    movesServer attr =
           (\l -> runApp (Move.indexMove (moveRepo repos) attr l))
      :<|> (\b -> runApp (Move.storeMove (moveRepo repos) attr b))
      :<|> (\i -> runApp (Move.showMove (moveRepo repos) attr i))
      :<|> (\i b -> runApp (Move.updateMove (moveRepo repos) attr i b))
      :<|> (\i -> runApp (Move.destroyMove (moveRepo repos) attr i))
    elementsServer attr =
           runApp (Element.indexElement (elementRepo repos) attr)
      :<|> (\b -> runApp (Element.storeElement (elementRepo repos) attr b))
      :<|> (\i -> runApp (Element.showElement (elementRepo repos) attr i))
      :<|> (\i b -> runApp (Element.updateElement (elementRepo repos) attr i b))
    categoriesServer attr =
           runApp (Category.indexCategory (categoryRepo repos) attr)
      :<|> (\b -> runApp (Category.storeCategory (categoryRepo repos) attr b))
      :<|> (\i -> runApp (Category.showCategory (categoryRepo repos) attr i))
      :<|> (\i b -> runApp (Category.updateCategory (categoryRepo repos) attr i b))
    templatesServer =
           runApp (Template.indexTemplate (templateRepo repos))
      :<|> (\b -> runApp (Template.storeTemplate (templateRepo repos) b))
      :<|> (\i -> runApp (Template.showTemplate (templateRepo repos) i))
      :<|> (\i b -> runApp (Template.updateTemplate (templateRepo repos) i b))
      :<|> (\i -> runApp (Template.destroyTemplate (templateRepo repos) i))

-- | 未定義パス / メソッド不一致 → E305 (既存 Route::fallback)。
fallback :: Tagged Handler Application
fallback = Tagged $ \_req respond ->
  respond (jsonResponse 404 "E305" "Page not found.")

-- | AppM (ExceptT AppError IO) を Servant Handler に変換。
-- 業務エラーは Left から HTTP へ、想定外例外は E999 に落とす。
runApp :: AppM Value -> Handler Value
runApp m = do
  r <- liftIO (try (runExceptT m) :: IO (Either SomeException (Either AppError Value)))
  case r of
    Right (Right v) -> pure v
    Right (Left appErr) -> throwError (toServerError appErr)
    Left e -> throwError (toServerError (AppError Unexpected (T.pack (show e))))

-- | 既存 CheckApiKey。Bearer トークンが API_KEY と一致しなければ 401 E201。
authMiddleware :: ByteString -> Middleware
authMiddleware key app req respond =
  case lookup hAuthorization (requestHeaders req) of
    Just v | Just tok <- BS.stripPrefix "Bearer " v, tok == key -> app req respond
    _ -> respond (jsonResponse 401 "E201" "Unauthorized")

application :: ByteString -> Repositories -> Application
application key repos = authMiddleware key (serve api (server repos))
