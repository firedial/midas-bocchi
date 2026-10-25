module Domain.Repository.Move
  ( MoveRepository (..)
  ) where

import Domain.Attribute (Attribute)
import Domain.Entities (Move, MoveInput)
import Domain.Error (AppError)

data MoveRepository m = MoveRepository
  { listMoves :: Attribute -> Maybe Int -> m (Either AppError [Move])
    -- ^ 一覧。ペアの金額和が 0 でなければ E902 を Left で返す。
  , findMove :: Attribute -> Int -> m (Either AppError (Maybe Move))
    -- ^ Right Nothing = 未検出、Left E902 = データ不整合。
  , createMove :: Attribute -> MoveInput -> m (Either AppError Move)
  , updateMove :: Attribute -> Int -> MoveInput -> m (Either AppError Move)
  , deleteMove :: Attribute -> Int -> m (Either AppError Move)
  }
