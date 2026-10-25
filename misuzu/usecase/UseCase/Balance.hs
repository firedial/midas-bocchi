-- | 収支のアプリケーションフロー。リポジトリ (ポート) 越しに操作し、
-- servant にも mysql にも依存しない。存在しなければ E301 を値で返す。
module UseCase.Balance
  ( list
  , get
  , store
  , update
  , destroy
  ) where

import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Control.Monad.Trans.Class (lift)

import Domain.Entities (Balance, BalanceInput)
import Domain.Error (AppError (..), ErrorCode (..))
import Domain.Repository.Balance (BalanceRepository (..))

notFound :: AppError
notFound = AppError RecordNotFound "Not found balance."

list :: Monad m => BalanceRepository m -> Maybe Int -> Bool -> m [Balance]
list repo limit orderDesc = listBalances repo limit orderDesc

get :: Monad m => BalanceRepository m -> Int -> m (Either AppError Balance)
get repo theId = runExceptT $ do
  mb <- lift (findBalance repo theId)
  maybe (throwError notFound) pure mb

store :: Monad m => BalanceRepository m -> BalanceInput -> m (Either AppError Balance)
store = createBalance

update :: Monad m => BalanceRepository m -> Int -> BalanceInput -> m (Either AppError Balance)
update repo theId input = runExceptT $ do
  mb <- lift (findBalance repo theId)
  maybe (throwError notFound) (const (ExceptT (updateBalance repo theId input))) mb

destroy :: Monad m => BalanceRepository m -> Int -> m (Either AppError Balance)
destroy repo theId = runExceptT $ do
  mb <- lift (findBalance repo theId)
  maybe (throwError notFound) (const (ExceptT (deleteBalance repo theId))) mb
