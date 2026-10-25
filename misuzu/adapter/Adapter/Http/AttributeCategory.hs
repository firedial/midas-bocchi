module Adapter.Http.AttributeCategory
  ( indexCategory
  , showCategory
  , storeCategory
  , updateCategory
  ) where

import Control.Monad (when)
import Control.Monad.Except (ExceptT (..), liftEither)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, object, toJSON, (.=))
import Data.Text (Text)

import Adapter.Http.Error (AppM)
import Adapter.Http.Validate
import Domain.Attribute (parseCategoryAttr)
import Domain.Entities (Category (..), CategoryInput (..))
import Domain.Error (AppError (..), ErrorCode (..))
import Domain.Repository.AttributeCategory (AttributeCategoryRepository)
import Domain.ValueObjects
import qualified UseCase.AttributeCategory as UC

categoryJson :: Category -> Value
categoryJson c = object
  [ "id" .= unId (catId c)
  , "name" .= unCategoryName (catName c)
  , "description" .= unDescription (catDescription c)
  ]

parseStore :: Value -> Either AppError CategoryInput
parseStore body = do
  let o = asObject body
  name <- requiredStr o "name"
  desc <- requiredStr o "description"
  name' <- mkCategoryName name
  desc' <- mkDescription desc
  pure (CategoryInput name' desc')

parseUpdate :: Int -> Value -> Either AppError CategoryInput
parseUpdate theId body = do
  let o = asObject body
  name <- requiredStr o "name"
  desc <- requiredStr o "description"
  when (isMoveId theId) (Left (AppError UsingMoveId "Can not update category move id."))
  name' <- mkCategoryName name
  desc' <- mkDescription desc
  pure (CategoryInput name' desc')

indexCategory :: AttributeCategoryRepository IO -> Text -> AppM Value
indexCategory repo attrName = do
  attr <- liftEither (parseCategoryAttr attrName)
  toJSON . map categoryJson <$> liftIO (UC.list repo attr)

showCategory :: AttributeCategoryRepository IO -> Text -> Int -> AppM Value
showCategory repo attrName theId = do
  attr <- liftEither (parseCategoryAttr attrName)
  _ <- liftEither (filledId theId)
  categoryJson <$> ExceptT (UC.get repo attr theId)

storeCategory :: AttributeCategoryRepository IO -> Text -> Value -> AppM Value
storeCategory repo attrName body = do
  attr <- liftEither (parseCategoryAttr attrName)
  input <- liftEither (parseStore body)
  categoryJson <$> ExceptT (UC.store repo attr input)

updateCategory :: AttributeCategoryRepository IO -> Text -> Int -> Value -> AppM Value
updateCategory repo attrName theId body = do
  attr <- liftEither (parseCategoryAttr attrName)
  input <- liftEither (parseUpdate theId body)
  categoryJson <$> ExceptT (UC.update repo attr theId input)
