module UseCase.Template
  ( list
  , get
  , store
  , update
  , destroy
  ) where

import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Control.Monad.Trans.Class (lift)

import Domain.Entities (Template, TemplateInput)
import Domain.Error (AppError (..), ErrorCode (..))
import Domain.Repository.Template (TemplateRepository (..))

notFound :: AppError
notFound = AppError RecordNotFound "Not found template."

list :: Monad m => TemplateRepository m -> m [Template]
list = listTemplates

get :: Monad m => TemplateRepository m -> Int -> m (Either AppError Template)
get repo theId = runExceptT $ do
  mt <- lift (findTemplate repo theId)
  maybe (throwError notFound) pure mt

store :: Monad m => TemplateRepository m -> TemplateInput -> m (Either AppError Template)
store = createTemplate

update :: Monad m => TemplateRepository m -> Int -> TemplateInput -> m (Either AppError Template)
update repo theId input = runExceptT $ do
  mt <- lift (findTemplate repo theId)
  maybe (throwError notFound) (const (ExceptT (updateTemplate repo theId input))) mt

destroy :: Monad m => TemplateRepository m -> Int -> m (Either AppError Template)
destroy repo theId = runExceptT $ do
  mt <- lift (findTemplate repo theId)
  maybe (throwError notFound) (const (ExceptT (deleteTemplate repo theId))) mt
