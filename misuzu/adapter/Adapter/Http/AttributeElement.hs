module Adapter.Http.AttributeElement
  ( indexElement
  , showElement
  , storeElement
  , updateElement
  ) where

import Control.Monad (when)
import Control.Monad.Except (ExceptT (..), liftEither)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, object, toJSON, (.=))
import Data.Text (Text)

import Adapter.Http.Error (AppM)
import Adapter.Http.Validate
import Domain.Attribute (parseElementAttr)
import Domain.Entities (Element (..), ElementInput (..))
import Domain.Error (AppError (..), ErrorCode (..))
import Domain.Repository.AttributeElement (AttributeElementRepository)
import Domain.ValueObjects
import qualified UseCase.AttributeElement as UC

elementJson :: Element -> Value
elementJson e = object
  [ "id" .= unId (elId e)
  , "name" .= unElementName (elName e)
  , "description" .= unDescription (elDescription e)
  , "priority" .= unPriority (elPriority e)
  , "category_id" .= unId (elCategoryId e)
  ]

parseStore :: Value -> Either AppError ElementInput
parseStore body = do
  let o = asObject body
  name <- reqStr o "name"
  desc <- reqStr o "description"
  priority <- reqInt o "priority"
  categoryId <- reqInt o "category_id"
  _ <- filledId categoryId
  when (isMoveId categoryId) (Left (AppError UsingMoveId "Attribute category id is move id."))
  name' <- mkElementName name
  desc' <- mkDescription desc
  priority' <- mkPriority priority
  pure (ElementInput name' desc' priority' (mkId categoryId))

parseUpdate :: Int -> Value -> Either AppError ElementInput
parseUpdate theId body = do
  let o = asObject body
  name <- reqStr o "name"
  desc <- reqStr o "description"
  priority <- reqInt o "priority"
  categoryId <- reqInt o "category_id"
  _ <- filledId categoryId
  when (isMoveId theId) (Left (AppError UsingMoveId "Can not update element move id."))
  when (isMoveId categoryId) (Left (AppError UsingMoveId "Attribute category id is move id."))
  name' <- mkElementName name
  desc' <- mkDescription desc
  priority' <- mkPriority priority
  pure (ElementInput name' desc' priority' (mkId categoryId))

indexElement :: AttributeElementRepository IO -> Text -> AppM Value
indexElement repo attrName = do
  attr <- liftEither (parseElementAttr attrName)
  toJSON . map elementJson <$> liftIO (UC.list repo attr)

showElement :: AttributeElementRepository IO -> Text -> Int -> AppM Value
showElement repo attrName theId = do
  attr <- liftEither (parseElementAttr attrName)
  _ <- liftEither (filledId theId)
  elementJson <$> ExceptT (UC.get repo attr theId)

storeElement :: AttributeElementRepository IO -> Text -> Value -> AppM Value
storeElement repo attrName body = do
  attr <- liftEither (parseElementAttr attrName)
  input <- liftEither (parseStore body)
  elementJson <$> ExceptT (UC.store repo attr input)

updateElement :: AttributeElementRepository IO -> Text -> Int -> Value -> AppM Value
updateElement repo attrName theId body = do
  attr <- liftEither (parseElementAttr attrName)
  input <- liftEither (parseUpdate theId body)
  elementJson <$> ExceptT (UC.update repo attr theId input)
