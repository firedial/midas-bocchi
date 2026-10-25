module UseCase.Move
  ( list
  , get
  , store
  , update
  , destroy
  ) where

import Control.Monad.Except (ExceptT (..), runExceptT, throwError)

import Domain.Attribute (Attribute)
import Domain.Entities (Move, MoveInput)
import Domain.Error (AppError (..), ErrorCode (..))
import Domain.Repository.Move (MoveRepository (..))

notFound :: AppError
notFound = AppError RecordNotFound "Not found move."

list :: Monad m => MoveRepository m -> Attribute -> Maybe Int -> m (Either AppError [Move])
list repo attr limit = listMoves repo attr limit

get :: Monad m => MoveRepository m -> Attribute -> Int -> m (Either AppError Move)
get repo attr theId = runExceptT $ do
  mv <- ExceptT (findMove repo attr theId)
  maybe (throwError notFound) pure mv

store :: Monad m => MoveRepository m -> Attribute -> MoveInput -> m (Either AppError Move)
store = createMove

update :: Monad m => MoveRepository m -> Attribute -> Int -> MoveInput -> m (Either AppError Move)
update repo attr theId input = runExceptT $ do
  mv <- ExceptT (findMove repo attr theId)
  maybe (throwError notFound) (const (ExceptT (updateMove repo attr theId input))) mv

destroy :: Monad m => MoveRepository m -> Attribute -> Int -> m (Either AppError Move)
destroy repo attr theId = runExceptT $ do
  mv <- ExceptT (findMove repo attr theId)
  maybe (throwError notFound) (const (ExceptT (deleteMove repo attr theId))) mv
