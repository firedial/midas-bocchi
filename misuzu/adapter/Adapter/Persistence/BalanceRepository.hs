-- | domain の BalanceRepository ポートを mysql-haskell で満たす。
-- 低レベルの SQL ヘルパ (selectJoined / *BalanceRaw / BalanceFilter) は
-- 移動 (MoveRepository) 実装からも再利用される。
module Adapter.Persistence.BalanceRepository
  ( newBalanceRepository
    -- 移動実装が再利用する低レベルヘルパ
  , BalanceFilter (..)
  , emptyFilter
  , selectJoined
  , insertBalanceRaw
  , updateBalanceRaw
  , deleteBalanceRaw
  ) where

import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Database.MySQL.Base (MySQLConn, MySQLValue)

import Adapter.Persistence.Db
import Domain.Entities (Balance (..), BalanceInput (..))
import Domain.Error (ErrorCode (..))
import Domain.Repository.Balance (BalanceRepository (..))
import Domain.ValueObjects
  ( Date
  , Item
  , dateText
  , mkAmount
  , mkDate
  , mkDescription
  , mkId
  , mkItem
  , unAmount
  , unId
  , unItem
  )

--------------------------------------------------------------------------------
-- ポート実装
--------------------------------------------------------------------------------

-- | 移動レコード (kind_element_id = 1) を除外して取得するための基底フィルタ。
baseFilter :: BalanceFilter
baseFilter = emptyFilter { fNotKind = Just 1 }

newBalanceRepository :: Db -> BalanceRepository IO
newBalanceRepository db = BalanceRepository
  { listBalances = \limit orderDesc ->
      withConn db $ \c -> withTx c $
        selectJoined c baseFilter { fLimit = limit, fOrderDesc = orderDesc }
  , findBalance = \theId ->
      withConn db $ \c -> withTx c $
        listToMaybe <$> selectJoined c baseFilter { fId = Just theId }
  , createBalance = \BalanceInput {..} ->
      tryApp $ withConn db $ \c -> withTx c $
        insertBalanceRaw c (unAmount biAmount) biItem (unId biKind) (unId biPurpose) (unId biPlace) biDate (unId <$> biGroupId)
  , updateBalance = \theId BalanceInput {..} ->
      tryApp $ withConn db $ \c -> withTx c $
        -- 更新時 group_id は adapter が必須検証済みで必ず Just。
        updateBalanceRaw c theId (unAmount biAmount) biItem (unId biKind) (unId biPurpose) (unId biPlace) biDate (maybe 0 unId biGroupId)
  , deleteBalance = \theId ->
      tryApp $ withConn db $ \c -> withTx c $ deleteBalanceRaw c theId
  }

--------------------------------------------------------------------------------
-- SELECT
--------------------------------------------------------------------------------

-- | m_balance の SELECT 条件 (既存 BalanceDataModel::selectBalance のパラメータ)。
data BalanceFilter = BalanceFilter
  { fNotKind :: Maybe Int
  , fPurpose :: Maybe Int
  , fPlace :: Maybe Int
  , fId :: Maybe Int
  , fLimit :: Maybe Int
  , fOrderDesc :: Bool
  }

emptyFilter :: BalanceFilter
emptyFilter = BalanceFilter Nothing Nothing Nothing Nothing Nothing False

joinedSelect :: Text
joinedSelect = T.unwords
  [ "SELECT m_balance.id, m_balance.amount, m_balance.item,"
  , "m_balance.kind_element_id, m_balance.purpose_element_id, m_balance.place_element_id,"
  , "m_balance.date, m_balance.group_id,"
  , "m_kind_element.description, m_purpose_element.description, m_place_element.description"
  , "FROM m_balance"
  , "JOIN m_kind_element ON m_kind_element.id = m_balance.kind_element_id"
  , "JOIN m_purpose_element ON m_purpose_element.id = m_balance.purpose_element_id"
  , "JOIN m_place_element ON m_place_element.id = m_balance.place_element_id"
  ]

-- | 既存 join 付き select。フィルタは NULL のものを無視する。
selectJoined :: MySQLConn -> BalanceFilter -> IO [Balance]
selectJoined conn BalanceFilter {..} = do
  let conds = catMaybes
        [ ("m_balance.kind_element_id <> ?",) <$> fNotKind
        , ("m_balance.purpose_element_id = ?",) <$> fPurpose
        , ("m_balance.place_element_id = ?",) <$> fPlace
        , ("m_balance.id = ?",) <$> fId
        ]
      whereClause
        | null conds = ""
        | otherwise = " WHERE " <> T.intercalate " AND " (map fst conds)
      orderClause = if fOrderDesc then " ORDER BY m_balance.id DESC" else ""
      limitClause = maybe "" (\n -> " LIMIT " <> T.pack (show n)) fLimit
      sql = joinedSelect <> whereClause <> orderClause <> limitClause
      params = map (pInt . snd) conds
  rows <- runQuery conn sql params
  mapM parseJoined rows

parseJoined :: [MySQLValue] -> IO Balance
parseJoined [i, a, it, k, p, pl, d, g, kd, pd, pld] = do
  item <- hydrate (mkItem (valText it))
  date <- hydrate (mkDate (valDateText d))
  kindDesc <- hydrate (mkDescription (valText kd))
  purposeDesc <- hydrate (mkDescription (valText pd))
  placeDesc <- hydrate (mkDescription (valText pld))
  pure Balance
    { balId = mkId (valInt i)
    , balAmount = mkAmount (valInt a)
    , balItem = item
    , balKind = mkId (valInt k)
    , balPurpose = mkId (valInt p)
    , balPlace = mkId (valInt pl)
    , balDate = date
    , balGroupId = mkId (valInt g)
    , balKindDesc = Just kindDesc
    , balPurposeDesc = Just purposeDesc
    , balPlaceDesc = Just placeDesc
    }
parseJoined row = error ("parseJoined: unexpected columns " <> show (length row))

-- | RETURNING * / SELECT * (m_balance のカラム順) からの生行。
parseRaw :: Int -> [MySQLValue] -> IO Balance
parseRaw groupId [i, a, it, k, p, pl, d, _g] = do
  item <- hydrate (mkItem (valText it))
  date <- hydrate (mkDate (valDateText d))
  pure Balance
    { balId = mkId (valInt i)
    , balAmount = mkAmount (valInt a)
    , balItem = item
    , balKind = mkId (valInt k)
    , balPurpose = mkId (valInt p)
    , balPlace = mkId (valInt pl)
    , balDate = date
    , balGroupId = mkId groupId
    , balKindDesc = Nothing
    , balPurposeDesc = Nothing
    , balPlaceDesc = Nothing
    }
parseRaw _ row = error ("parseRaw: unexpected columns " <> show (length row))

firstRow :: Text -> [[MySQLValue]] -> IO [MySQLValue]
firstRow ctx = \case
  (r : _) -> pure r
  [] -> throwApp Unexpected (ctx <> ": no row returned")

-- | 挿入・更新後の値から desc なしの Balance を組み立てる。
-- item/date は検証済み VO を受け取り、id/amount はここで薄くラップする。
rawBalance :: Int -> Int -> Item -> Int -> Int -> Int -> Date -> Int -> Balance
rawBalance theId amount item kind purpose place date groupId = Balance
  { balId = mkId theId
  , balAmount = mkAmount amount
  , balItem = item
  , balKind = mkId kind
  , balPurpose = mkId purpose
  , balPlace = mkId place
  , balDate = date
  , balGroupId = mkId groupId
  , balKindDesc = Nothing
  , balPurposeDesc = Nothing
  , balPlaceDesc = Nothing
  }

--------------------------------------------------------------------------------
-- INSERT / UPDATE / DELETE
--------------------------------------------------------------------------------

-- | 既存 BalanceDataModel::insertBalance を移植。group_id 未指定なら挿入後に id で埋める。
insertBalanceRaw
  :: MySQLConn
  -> Int -> Item -> Int -> Int -> Int -> Date -> Maybe Int
  -> IO Balance
insertBalanceRaw conn amount item kind purpose place date mGroup =
  handleQueryException "Insert balance error." $ do
    let sql = T.unwords
          [ "INSERT INTO m_balance"
          , "(amount, item, kind_element_id, purpose_element_id, place_element_id, date, group_id)"
          , "VALUES (?, ?, ?, ?, ?, ?, ?)"
          ]
        params = [pInt amount, pText (unItem item), pInt kind, pInt purpose, pInt place, pDate (dateText date), maybe pNull pInt mGroup]
    ok <- runExec conn sql params
    let newId = lastInsertId ok
    finalGroup <- case mGroup of
      Just g -> pure g
      Nothing -> do
        _ <- runExec conn "UPDATE m_balance SET group_id = ? WHERE id = ?" [pInt newId, pInt newId]
        pure newId
    pure (rawBalance newId amount item kind purpose place date finalGroup)

-- | 既存 BalanceDataModel::updateBalance を移植。
updateBalanceRaw
  :: MySQLConn
  -> Int -> Int -> Item -> Int -> Int -> Int -> Date -> Int
  -> IO Balance
updateBalanceRaw conn theId amount item kind purpose place date groupId =
  handleQueryException "Update balance error." $ do
    let updSql = T.unwords
          [ "UPDATE m_balance SET"
          , "amount = ?, item = ?, kind_element_id = ?, purpose_element_id = ?,"
          , "place_element_id = ?, date = ?, group_id = ?"
          , "WHERE id = ?"
          ]
        params = [pInt amount, pText (unItem item), pInt kind, pInt purpose, pInt place, pDate (dateText date), pInt groupId, pInt theId]
    _ <- runExec conn updSql params
    pure (rawBalance theId amount item kind purpose place date groupId)

-- | 既存 BalanceDataModel::deleteBalance を移植。削除前の行を SELECT して返す。
deleteBalanceRaw :: MySQLConn -> Int -> IO Balance
deleteBalanceRaw conn theId =
  handleQueryException "Delete balance error." $ do
    row <- firstRow "deleteBalance" =<< runQuery conn "SELECT * FROM m_balance WHERE id = ?" [pInt theId]
    let groupId = valInt (row !! 7)
    _ <- runExec conn "DELETE FROM m_balance WHERE id = ?" [pInt theId]
    parseRaw groupId row
