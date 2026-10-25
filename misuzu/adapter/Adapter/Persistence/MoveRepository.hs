-- | domain の MoveRepository ポートを満たす。移動は m_balance の 2 行ペアで表現され、
-- Balance の低レベルヘルパを再利用して 1 トランザクション内で原子的に扱う。
module Adapter.Persistence.MoveRepository
  ( newMoveRepository
  ) where

import Control.Monad (when)
import Database.MySQL.Base (MySQLConn)

import Adapter.Persistence.BalanceRepository
import Adapter.Persistence.Db (Db, throwApp, tryApp, withConn, withTx)
import Domain.Attribute (Attribute (..), isPurpose)
import Domain.Entities (Balance (..), Move (..), MoveInput (..))
import Domain.Error (ErrorCode (..))
import Domain.Repository.Move (MoveRepository (..))
import Domain.ValueObjects (Description, Id, unAmount, unId)

newMoveRepository :: Db -> MoveRepository IO
newMoveRepository db = MoveRepository
  { listMoves = \attr limit ->
      tryApp $ withConn db $ \c -> withTx c $ getMovesImpl c attr limit
  , findMove = \attr theId ->
      tryApp $ withConn db $ \c -> withTx c $ selectMoveImpl c attr theId
  , createMove = \attr input ->
      tryApp $ withConn db $ \c -> withTx c $ insertMoveImpl c attr input
  , updateMove = \attr theId input ->
      tryApp $ withConn db $ \c -> withTx c $ updateMoveImpl c attr theId input
  , deleteMove = \attr theId ->
      tryApp $ withConn db $ \c -> withTx c $ deleteMoveImpl c attr theId
  }

-- | 移動対象の反対属性を move_id (=1) に固定するフィルタ。
oppFilter :: Attribute -> BalanceFilter
oppFilter Purpose = emptyFilter { fPlace = Just 1 }
oppFilter Place = emptyFilter { fPurpose = Just 1 }
oppFilter Kind = emptyFilter

-- | 属性に応じて before/after 要素 ID と description を取り出す。
elemId :: Attribute -> Balance -> Id
elemId attr b = if isPurpose attr then balPurpose b else balPlace b

elemDesc :: Attribute -> Balance -> Maybe Description
elemDesc attr b = if isPurpose attr then balPurposeDesc b else balPlaceDesc b

buildMove :: Attribute -> Balance -> Balance -> Move
buildMove attr before after = Move
  { mvId = balId before
  , mvAmount = balAmount after
  , mvItem = balItem before
  , mvBeforeId = elemId attr before
  , mvAfterId = elemId attr after
  , mvDate = balDate before
  , mvGroupId = balGroupId before
  , mvBeforeDesc = elemDesc attr before
  , mvAfterDesc = elemDesc attr after
  }

-- | 移動前後の purpose/place 要素 ID を組み立てる。
moveElements :: Attribute -> Int -> Int -> (Int, Int, Int, Int)
moveElements Purpose before after = (before, after, 1, 1) -- (beforePurpose, afterPurpose, beforePlace, afterPlace)
moveElements Place before after = (1, 1, before, after)
moveElements Kind _ _ = (1, 1, 1, 1)

getMovesImpl :: MySQLConn -> Attribute -> Maybe Int -> IO [Move]
getMovesImpl conn attr limit = do
  let filt = (oppFilter attr) { fLimit = (* 2) <$> limit, fOrderDesc = True }
  rows <- selectJoined conn filt
  buildFromPairs attr (pairs rows)
  where
    pairs (a : b : rest) = (a, b) : pairs rest
    pairs _ = []

buildFromPairs :: Attribute -> [(Balance, Balance)] -> IO [Move]
buildFromPairs _ [] = pure []
buildFromPairs attr ((after, before) : rest)
  | unAmount (balAmount before) >= 0 = buildFromPairs attr rest
  | unAmount (balAmount before) /= negate (unAmount (balAmount after)) =
      throwApp UnexpectedAmount "Move amount sum is not zero."
  | otherwise = (buildMove attr before after :) <$> buildFromPairs attr rest

-- | 未検出は Nothing、金額和不整合は E902。before が正の行 (=移動でない) も未検出扱い。
selectMoveImpl :: MySQLConn -> Attribute -> Int -> IO (Maybe Move)
selectMoveImpl conn attr moveId = do
  befores <- selectJoined conn (oppFilter attr) { fId = Just moveId }
  afters <- selectJoined conn (oppFilter attr) { fId = Just (moveId + 1) }
  case (befores, afters) of
    (before : _, after : _)
      | unAmount (balAmount before) >= 0 -> pure Nothing
      | unAmount (balAmount before) /= negate (unAmount (balAmount after)) -> throwApp UnexpectedAmount "Move amount sum is not zero."
      | otherwise -> pure (Just (buildMove attr before after))
    _ -> pure Nothing

insertMoveImpl :: MySQLConn -> Attribute -> MoveInput -> IO Move
insertMoveImpl conn attr MoveInput {..} = do
  let amount = unAmount miAmount
  when (amount < 0) (throwApp UnexpectedAmount "Move amount needs positive amount.")
  let (bp, ap', bpl, apl) = moveElements attr (unId miBeforeId) (unId miAfterId)
      mGroup = unId <$> miGroupId
  beforeBal <- insertBalanceRaw conn (negate amount) miItem 1 bp bpl miDate mGroup
  let bid = unId (balId beforeBal)
  afterBal <- insertBalanceRaw conn amount miItem 1 ap' apl miDate (Just (maybe bid id mGroup))
  when (unId (balId afterBal) /= bid + 1) (throwApp UnexpectedDifferenceIdMove "After id is wrong.")
  pure (buildMove attr beforeBal afterBal)

updateMoveImpl :: MySQLConn -> Attribute -> Int -> MoveInput -> IO Move
updateMoveImpl conn attr moveId MoveInput {..} = do
  let amount = unAmount miAmount
  when (amount < 0) (throwApp UnexpectedAmount "Move amount needs positive amount.")
  let (bp, ap', bpl, apl) = moveElements attr (unId miBeforeId) (unId miAfterId)
      -- 更新時 group_id は adapter が必須検証済みで必ず Just。
      groupId = maybe 0 unId miGroupId
  beforeBal <- updateBalanceRaw conn moveId (negate amount) miItem 1 bp bpl miDate groupId
  afterBal <- updateBalanceRaw conn (moveId + 1) amount miItem 1 ap' apl miDate groupId
  pure (buildMove attr beforeBal afterBal)

deleteMoveImpl :: MySQLConn -> Attribute -> Int -> IO Move
deleteMoveImpl conn attr moveId = do
  beforeBal <- deleteBalanceRaw conn moveId
  afterBal <- deleteBalanceRaw conn (moveId + 1)
  pure (buildMove attr beforeBal afterBal)
