module Adapter.Http.Template
  ( indexTemplate
  , showTemplate
  , storeTemplate
  , updateTemplate
  , destroyTemplate
  ) where

import Control.Monad (when)
import Control.Monad.Except (ExceptT (..), liftEither)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Object, Value (..), object, toJSON, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Adapter.Http.Error (AppM)
import Adapter.Http.Validate (asObject, intOfJson, lookupField, requiredInt, requiredStr)
import Domain.Entities (Template (..), TemplateDetail (..), TemplateInput (..))
import Domain.Error (AppError (..), ErrorCode (..))
import Domain.Repository.Template (TemplateRepository)
import Domain.ValueObjects
  ( Amount
  , Item
  , filledId
  , mkAmount
  , mkId
  , mkItem
  , mkTemplateName
  , unAmount
  , unId
  , unItem
  , unTemplateName
  )
import qualified UseCase.Template as UC

--------------------------------------------------------------------------------
-- シリアライズ
--------------------------------------------------------------------------------

templateSummary :: Template -> Value
templateSummary t = object ["id" .= unId (tmplId t), "name" .= unTemplateName (tmplName t)]

templateDetailJson :: Template -> Value
templateDetailJson t = object
  [ "id" .= unId (tmplId t)
  , "name" .= unTemplateName (tmplName t)
  , "details" .= map detailJson (tmplDetails t)
  ]

detailJson :: TemplateDetail -> Value
detailJson d = object
  [ "seq" .= tdSeq d
  , "type" .= tdType d
  , "amount" .= unAmount (tdAmount d)
  , "item" .= unItem (tdItem d)
  , "kind_element_id" .= (unId <$> tdKindElementId d)
  , "purpose_element_id" .= (unId <$> tdPurposeElementId d)
  , "place_element_id" .= (unId <$> tdPlaceElementId d)
  , "move_before_purpose_id" .= (unId <$> tdMoveBeforePurposeId d)
  , "move_after_purpose_id" .= (unId <$> tdMoveAfterPurposeId d)
  , "move_before_place_id" .= (unId <$> tdMoveBeforePlaceId d)
  , "move_after_place_id" .= (unId <$> tdMoveAfterPlaceId d)
  ]

--------------------------------------------------------------------------------
-- ハンドラ
--------------------------------------------------------------------------------

indexTemplate :: TemplateRepository IO -> AppM Value
indexTemplate repo = toJSON . map templateSummary <$> liftIO (UC.list repo)

showTemplate :: TemplateRepository IO -> Int -> AppM Value
showTemplate repo theId = do
  _ <- liftEither (filledId theId)
  templateDetailJson <$> ExceptT (UC.get repo theId)

storeTemplate :: TemplateRepository IO -> Value -> AppM Value
storeTemplate repo body = do
  input <- liftEither (validateTemplate body)
  templateSummary <$> ExceptT (UC.store repo input)

updateTemplate :: TemplateRepository IO -> Int -> Value -> AppM Value
updateTemplate repo theId body = do
  _ <- liftEither (filledId theId)
  input <- liftEither (validateTemplate body)
  templateSummary <$> ExceptT (UC.update repo theId input)

destroyTemplate :: TemplateRepository IO -> Int -> AppM Value
destroyTemplate repo theId = do
  _ <- liftEither (filledId theId)
  templateSummary <$> ExceptT (UC.destroy repo theId)

--------------------------------------------------------------------------------
-- バリデーション (既存 TemplateController::validateAndBuildDetails 移植)
--------------------------------------------------------------------------------

validateTemplate :: Value -> Either AppError TemplateInput
validateTemplate body = do
  let o = asObject body
  name <- requiredStr o "name"
  when (T.length name > 20) (Left (AppError InvalidLength "name is too long"))
  detailVals <- case lookupField "details" o of
    Nothing -> Left (AppError MissingRequired "details is required")
    Just Null -> Left (AppError MissingRequired "details is required")
    Just (Array arr)
      | V.null arr -> Left (AppError MissingRequired "details must have at least one item")
      | otherwise -> Right (V.toList arr)
    Just _ -> Left (AppError MissingRequired "details is required")
  details <- traverse buildDetail (zip [1 ..] detailVals)
  pure (TemplateInput (mkTemplateName name) details)

buildDetail :: (Int, Value) -> Either AppError TemplateDetail
buildDetail (seq', val) = do
  let d = asObject val
  ty <- requiredInt d "type"
  when (ty `notElem` [1, 2, 3]) (Left (AppError InvalidValue "type is not a valid value"))
  amount <- requiredInt d "amount"
  item <- mkItem =<< requiredStr d "item"
  case ty of
    1 -> do
      kind <- requireIntField "kind_element_id" d
      purpose <- requireIntField "purpose_element_id" d
      place <- requireIntField "place_element_id" d
      requireNullField "move_before_purpose_id" d
      requireNullField "move_after_purpose_id" d
      requireNullField "move_before_place_id" d
      requireNullField "move_after_place_id" d
      pure (emptyDetail seq' 1 (mkAmount amount) item)
        { tdKindElementId = Just (mkId kind)
        , tdPurposeElementId = Just (mkId purpose)
        , tdPlaceElementId = Just (mkId place)
        }
    2 -> do
      when (amount < 0) (Left (AppError InvalidRange "amount must not be negative"))
      requireNullField "purpose_element_id" d
      requireNullField "place_element_id" d
      mbp <- requireIntField "move_before_purpose_id" d
      map' <- requireIntField "move_after_purpose_id" d
      requireNullField "move_before_place_id" d
      requireNullField "move_after_place_id" d
      pure (emptyDetail seq' 2 (mkAmount amount) item)
        { tdKindElementId = mkId <$> optIntField "kind_element_id" d
        , tdMoveBeforePurposeId = Just (mkId mbp)
        , tdMoveAfterPurposeId = Just (mkId map')
        }
    _ -> do
      when (amount < 0) (Left (AppError InvalidRange "amount must not be negative"))
      requireNullField "purpose_element_id" d
      requireNullField "place_element_id" d
      requireNullField "move_before_purpose_id" d
      requireNullField "move_after_purpose_id" d
      mbp <- requireIntField "move_before_place_id" d
      map' <- requireIntField "move_after_place_id" d
      pure (emptyDetail seq' 3 (mkAmount amount) item)
        { tdKindElementId = mkId <$> optIntField "kind_element_id" d
        , tdMoveBeforePlaceId = Just (mkId mbp)
        , tdMoveAfterPlaceId = Just (mkId map')
        }

emptyDetail :: Int -> Int -> Amount -> Item -> TemplateDetail
emptyDetail seq' ty amount item = TemplateDetail
  { tdSeq = seq'
  , tdType = ty
  , tdAmount = amount
  , tdItem = item
  , tdKindElementId = Nothing
  , tdPurposeElementId = Nothing
  , tdPlaceElementId = Nothing
  , tdMoveBeforePurposeId = Nothing
  , tdMoveAfterPurposeId = Nothing
  , tdMoveBeforePlaceId = Nothing
  , tdMoveAfterPlaceId = Nothing
  }

-- | 既存 is_int チェック相当。整数でなければ E109。
requireIntField :: Text -> Object -> Either AppError Int
requireIntField field o = case lookupField field o >>= intOfJson of
  Just i -> Right i
  Nothing -> Left (AppError MissingRequired (field <> " is required"))

-- | 既存 is_null チェック相当。null でも欠損でもなければ E106。
requireNullField :: Text -> Object -> Either AppError ()
requireNullField field o = case lookupField field o of
  Nothing -> Right ()
  Just Null -> Right ()
  Just _ -> Left (AppError InvalidValue (field <> " must be null"))

-- | 型チェックなしの任意整数 (整数なら Just、それ以外は Nothing)。
optIntField :: Text -> Object -> Maybe Int
optIntField field o = lookupField field o >>= intOfJson
