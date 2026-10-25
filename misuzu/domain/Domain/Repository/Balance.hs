-- | 依存性逆転の要。domain は「収支の永続化に何ができるか」の型だけを定義し実装は持たない。
-- m を型引数にすることで、本番は IO、テストは Identity/State を差し込める。
module Domain.Repository.Balance
  ( BalanceRepository (..)
  ) where

import Domain.Entities (Balance, BalanceInput)
import Domain.Error (AppError)

data BalanceRepository m = BalanceRepository
  { listBalances :: Maybe Int -> Bool -> m [Balance]
    -- ^ 移動レコードを除いた収支一覧 (limit, 降順フラグ)。
  , findBalance :: Int -> m (Maybe Balance)
    -- ^ 移動レコードを除いた 1 件取得。
  , createBalance :: BalanceInput -> m (Either AppError Balance)
    -- ^ 失敗 (FK 不在→E302 / 重複→E304) は Left で返す。
  , updateBalance :: Int -> BalanceInput -> m (Either AppError Balance)
  , deleteBalance :: Int -> m (Either AppError Balance)
  }
