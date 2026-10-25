module UseCase.AttributeElement
  ( list
  , get
  , store
  , update
  ) where

import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Control.Monad.Trans.Class (lift)

import Domain.Attribute (Attribute)
import Domain.Entities (Element, ElementInput)
import Domain.Error (AppError (..), ErrorCode (..))
import Domain.Repository.AttributeElement (AttributeElementRepository (..))

notFound :: AppError
notFound = AppError RecordNotFound "Not found attribute element."

list :: Monad m => AttributeElementRepository m -> Attribute -> m [Element]
list repo attr = listElements repo attr

get :: Monad m => AttributeElementRepository m -> Attribute -> Int -> m (Either AppError Element)
get repo attr theId = runExceptT $ do
  me <- lift (findElement repo attr theId)
  maybe (throwError notFound) pure me

store :: Monad m => AttributeElementRepository m -> Attribute -> ElementInput -> m (Either AppError Element)
store = createElement

update :: Monad m => AttributeElementRepository m -> Attribute -> Int -> ElementInput -> m (Either AppError Element)
update repo attr theId input = runExceptT $ do
  me <- lift (findElement repo attr theId)
  maybe (throwError notFound) (const (ExceptT (updateElement repo attr theId input))) me
