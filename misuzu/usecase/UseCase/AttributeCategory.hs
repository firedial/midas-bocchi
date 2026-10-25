module UseCase.AttributeCategory
  ( list
  , get
  , store
  , update
  ) where

import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Control.Monad.Trans.Class (lift)

import Domain.Attribute (Attribute)
import Domain.Entities (Category, CategoryInput)
import Domain.Error (AppError (..), ErrorCode (..))
import Domain.Repository.AttributeCategory (AttributeCategoryRepository (..))

notFound :: AppError
notFound = AppError RecordNotFound "Not found attribute category."

list :: Monad m => AttributeCategoryRepository m -> Attribute -> m [Category]
list repo attr = listCategories repo attr

get :: Monad m => AttributeCategoryRepository m -> Attribute -> Int -> m (Either AppError Category)
get repo attr theId = runExceptT $ do
  mc <- lift (findCategory repo attr theId)
  maybe (throwError notFound) pure mc

store :: Monad m => AttributeCategoryRepository m -> Attribute -> CategoryInput -> m (Either AppError Category)
store = createCategory

update :: Monad m => AttributeCategoryRepository m -> Attribute -> Int -> CategoryInput -> m (Either AppError Category)
update repo attr theId input = runExceptT $ do
  mc <- lift (findCategory repo attr theId)
  maybe (throwError notFound) (const (ExceptT (updateCategory repo attr theId input))) mc
