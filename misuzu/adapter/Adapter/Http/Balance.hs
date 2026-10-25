-- | 収支の HTTP アダプタ。リクエストを検証済みコマンド (BalanceInput) に変換し、
-- UseCase を呼んで結果を JSON 化する。業務ロジックは UseCase 側にある。
module Adapter.Http.Balance
  ( indexBalance
  , showBalance
  , storeBalance
  , updateBalance
  , destroyBalance
  ) where

import Control.Monad (when)
import Control.Monad.Except (ExceptT (..), liftEither)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, object, toJSON, (.=))
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Adapter.Http.Error (AppM)
import Adapter.Http.Validate
import Domain.Entities (Balance (..), BalanceInput (..))
import Domain.Error (AppError (..), ErrorCode (..))
import Domain.Repository.Balance (BalanceRepository)
import Domain.ValueObjects
import qualified UseCase.Balance as UC

--------------------------------------------------------------------------------
-- シリアライズ
--------------------------------------------------------------------------------

balanceWithDesc :: Balance -> Value
balanceWithDesc b = object
  [ "id" .= unId (balId b)
  , "amount" .= unAmount (balAmount b)
  , "item" .= unItem (balItem b)
  , "kind_element_id" .= unId (balKind b)
  , "purpose_element_id" .= unId (balPurpose b)
  , "place_element_id" .= unId (balPlace b)
  , "group_id" .= unId (balGroupId b)
  , "kind_element_description" .= maybe "" unDescription (balKindDesc b)
  , "purpose_element_description" .= maybe "" unDescription (balPurposeDesc b)
  , "place_element_description" .= maybe "" unDescription (balPlaceDesc b)
  , "date" .= dateText (balDate b)
  ]

balanceNoDesc :: Balance -> Value
balanceNoDesc b = object
  [ "id" .= unId (balId b)
  , "amount" .= unAmount (balAmount b)
  , "item" .= unItem (balItem b)
  , "kind_element_id" .= unId (balKind b)
  , "purpose_element_id" .= unId (balPurpose b)
  , "place_element_id" .= unId (balPlace b)
  , "group_id" .= unId (balGroupId b)
  , "date" .= dateText (balDate b)
  ]

--------------------------------------------------------------------------------
-- 検証 (既存ハンドラの順序を保持)
--------------------------------------------------------------------------------

parseStore :: Value -> Either AppError BalanceInput
parseStore body = do
  let o = asObject body
  amount <- reqInt o "amount"
  kind <- reqInt o "kind_element_id"
  purpose <- reqInt o "purpose_element_id"
  place <- reqInt o "place_element_id"
  item <- reqStr o "item"
  date <- reqStr o "date"
  mGroup <- optNullableInt o "group_id"
  checkGroupMin mGroup
  item' <- mkItem item
  _ <- filledId kind
  _ <- filledId purpose
  _ <- filledId place
  dateObj <- mkDate date
  checkElementNotMove kind purpose place
  checkAmountNonZero amount
  pure (BalanceInput (mkAmount amount) item' (mkId kind) (mkId purpose) (mkId place) dateObj (mkId <$> mGroup))

parseUpdate :: Value -> Either AppError BalanceInput
parseUpdate body = do
  let o = asObject body
  amount <- reqInt o "amount"
  kind <- reqInt o "kind_element_id"
  purpose <- reqInt o "purpose_element_id"
  place <- reqInt o "place_element_id"
  item <- reqStr o "item"
  date <- reqStr o "date"
  groupId <- reqInt o "group_id"
  when (groupId < 1) (Left (AppError InvalidRange "group_id must be at least 1"))
  item' <- mkItem item
  _ <- filledId kind
  _ <- filledId purpose
  _ <- filledId place
  dateObj <- mkDate date
  checkElementNotMove kind purpose place
  checkAmountNonZero amount
  pure (BalanceInput (mkAmount amount) item' (mkId kind) (mkId purpose) (mkId place) dateObj (Just (mkId groupId)))

--------------------------------------------------------------------------------
-- ハンドラ
--------------------------------------------------------------------------------

indexBalance :: BalanceRepository IO -> Maybe Text -> Maybe Text -> AppM Value
indexBalance repo mLimit mOrder = do
  limit <- liftEither (parseLimit mLimit)
  orderDesc <- liftEither (parseOrderBy mOrder)
  balances <- liftIO (UC.list repo limit orderDesc)
  pure (toJSON (map balanceWithDesc balances))

showBalance :: BalanceRepository IO -> Int -> AppM Value
showBalance repo theId = do
  _ <- liftEither (filledId theId)
  balanceWithDesc <$> ExceptT (UC.get repo theId)

storeBalance :: BalanceRepository IO -> Value -> AppM Value
storeBalance repo body = do
  input <- liftEither (parseStore body)
  balanceNoDesc <$> ExceptT (UC.store repo input)

updateBalance :: BalanceRepository IO -> Int -> Value -> AppM Value
updateBalance repo theId body = do
  _ <- liftEither (filledId theId)
  input <- liftEither (parseUpdate body)
  balanceNoDesc <$> ExceptT (UC.update repo theId input)

destroyBalance :: BalanceRepository IO -> Int -> AppM Value
destroyBalance repo theId = do
  _ <- liftEither (filledId theId)
  balanceNoDesc <$> ExceptT (UC.destroy repo theId)
