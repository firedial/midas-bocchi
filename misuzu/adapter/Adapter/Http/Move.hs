module Adapter.Http.Move
  ( indexMove
  , showMove
  , storeMove
  , updateMove
  , destroyMove
  ) where

import Control.Monad.Except (ExceptT (..), liftEither)
import Data.Aeson (Value, object, toJSON, (.=))
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Adapter.Http.Error (AppM)
import Adapter.Http.Validate
import Domain.Attribute (parseMoveAttr)
import Domain.Entities (Move (..), MoveInput (..))
import Domain.Error (AppError)
import Domain.Repository.Move (MoveRepository)
import Domain.ValueObjects
import qualified UseCase.Move as UC

--------------------------------------------------------------------------------
-- シリアライズ
--------------------------------------------------------------------------------

moveWithDesc :: Move -> Value
moveWithDesc m = object
  [ "id" .= unId (mvId m)
  , "amount" .= unAmount (mvAmount m)
  , "item" .= unItem (mvItem m)
  , "before_id" .= unId (mvBeforeId m)
  , "after_id" .= unId (mvAfterId m)
  , "date" .= dateText (mvDate m)
  , "group_id" .= unId (mvGroupId m)
  , "before_description" .= maybe "" unDescription (mvBeforeDesc m)
  , "after_description" .= maybe "" unDescription (mvAfterDesc m)
  ]

moveNoDesc :: Move -> Value
moveNoDesc m = object
  [ "id" .= unId (mvId m)
  , "amount" .= unAmount (mvAmount m)
  , "item" .= unItem (mvItem m)
  , "before_id" .= unId (mvBeforeId m)
  , "after_id" .= unId (mvAfterId m)
  , "date" .= dateText (mvDate m)
  , "group_id" .= unId (mvGroupId m)
  ]

--------------------------------------------------------------------------------
-- 検証 (既存ハンドラの順序を保持)
--------------------------------------------------------------------------------

parseStore :: Value -> Either AppError MoveInput
parseStore body = do
  let o = asObject body
  amount <- reqInt o "amount"
  requireMin1 "amount" amount
  item <- reqStr o "item"
  before <- reqInt o "before_id"
  after <- reqInt o "after_id"
  date <- reqStr o "date"
  mGroup <- optNullableInt o "group_id"
  checkGroupMin mGroup
  _ <- filledId before
  _ <- filledId after
  checkMoveElements before after
  item' <- mkItem item
  dateObj <- mkDate date
  pure (MoveInput (mkAmount amount) item' (mkId before) (mkId after) dateObj (mkId <$> mGroup))

parseUpdate :: Value -> Either AppError MoveInput
parseUpdate body = do
  let o = asObject body
  amount <- reqInt o "amount"
  requireMin1 "amount" amount
  item <- reqStr o "item"
  before <- reqInt o "before_id"
  after <- reqInt o "after_id"
  date <- reqStr o "date"
  groupId <- reqInt o "group_id"
  requireMin1 "group_id" groupId
  _ <- filledId before
  _ <- filledId after
  checkMoveElements before after
  item' <- mkItem item
  dateObj <- mkDate date
  pure (MoveInput (mkAmount amount) item' (mkId before) (mkId after) dateObj (Just (mkId groupId)))

--------------------------------------------------------------------------------
-- ハンドラ
--------------------------------------------------------------------------------

indexMove :: MoveRepository IO -> Text -> Maybe Text -> AppM Value
indexMove repo attrName mLimit = do
  attr <- liftEither (parseMoveAttr attrName)
  limit <- liftEither (parseLimit mLimit)
  moves <- ExceptT (UC.list repo attr limit)
  pure (toJSON (map moveWithDesc moves))

showMove :: MoveRepository IO -> Text -> Int -> AppM Value
showMove repo attrName theId = do
  attr <- liftEither (parseMoveAttr attrName)
  _ <- liftEither (filledId theId)
  moveWithDesc <$> ExceptT (UC.get repo attr theId)

storeMove :: MoveRepository IO -> Text -> Value -> AppM Value
storeMove repo attrName body = do
  attr <- liftEither (parseMoveAttr attrName)
  input <- liftEither (parseStore body)
  moveNoDesc <$> ExceptT (UC.store repo attr input)

updateMove :: MoveRepository IO -> Text -> Int -> Value -> AppM Value
updateMove repo attrName theId body = do
  attr <- liftEither (parseMoveAttr attrName)
  _ <- liftEither (filledId theId)
  input <- liftEither (parseUpdate body)
  moveNoDesc <$> ExceptT (UC.update repo attr theId input)

destroyMove :: MoveRepository IO -> Text -> Int -> AppM Value
destroyMove repo attrName theId = do
  attr <- liftEither (parseMoveAttr attrName)
  _ <- liftEither (filledId theId)
  moveNoDesc <$> ExceptT (UC.destroy repo attr theId)
