-- | 接続プール・トランザクション・値変換・例外写像。
-- 永続化層の内部では AppError を例外として投げ (throwApp)、リポジトリメソッドの
-- 境界で tryApp によって Either へ畳み込む。ポート (domain) には Either だけが見える。
module Adapter.Persistence.Db
  ( Db
  , DbConnectInfo (..)
  , newDb
  , withConn
  , withTx
  , runQuery
  , runQuery_
  , runExec
  , lastInsertId
  , handleQueryException
  , throwApp
  , tryApp
  , hydrate
  -- value helpers
  , valInt
  , valIntMaybe
  , valText
  , valDateText
  , pInt
  , pText
  , pDate
  , pNull
  ) where

import Control.Exception (catch, throwIO, try)
import Data.ByteString (ByteString)
import Data.Pool (Pool, defaultPoolConfig, newPool, setNumStripes, withResource)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, formatTime)

import Database.MySQL.Base
  ( ConnectInfo (..)
  , ERRException (..)
  , MySQLConn
  , MySQLValue (..)
  , OK
  , close
  , connect
  , defaultConnectInfo
  , errCode
  , execute
  , okLastInsertID
  , query
  , withTransaction
  )
import qualified System.IO.Streams as Streams

import Domain.Error (AppError (..), ErrorCode (..))

newtype Db = Db (Pool MySQLConn)

-- | 接続情報。app (Config) に依存しないよう素の値だけを受け取る。
data DbConnectInfo = DbConnectInfo
  { dbHost :: String
  , dbPort :: Int
  , dbUser :: ByteString
  , dbPass :: ByteString
  , dbName :: ByteString
  }

newDb :: DbConnectInfo -> IO Db
newDb DbConnectInfo {..} = do
  let ci = defaultConnectInfo
        { ciHost = dbHost
        , ciPort = fromIntegral dbPort
        , ciUser = dbUser
        , ciPassword = dbPass
        , ciDatabase = dbName
        , ciCharset = 45 -- utf8mb4_general_ci
        }
  pool <- newPool (setNumStripes (Just 1) (defaultPoolConfig (connect ci) close 60 8))
  pure (Db pool)

withConn :: Db -> (MySQLConn -> IO a) -> IO a
withConn (Db pool) = withResource pool

-- | 既存 Usecase の DB::beginTransaction/commit/rollBack 相当。
withTx :: MySQLConn -> IO a -> IO a
withTx = withTransaction

-- | パラメータ付きクエリを実行し行のリストを返す。
runQuery :: MySQLConn -> Text -> [MySQLValue] -> IO [[MySQLValue]]
runQuery conn sql params = do
  (_, is) <- query conn (fromString (T.unpack sql)) params
  Streams.toList is

runQuery_ :: MySQLConn -> Text -> IO [[MySQLValue]]
runQuery_ conn sql = runQuery conn sql []

-- | 結果セットを返さない書き込み。
runExec :: MySQLConn -> Text -> [MySQLValue] -> IO OK
runExec conn sql params = execute conn (fromString (T.unpack sql)) params

-- | 直近の INSERT で採番された ID。
lastInsertId :: OK -> Int
lastInsertId = okLastInsertID

-- | 永続化層で業務エラーを送出する (境界の tryApp で Either に畳まれる)。
throwApp :: ErrorCode -> Text -> IO a
throwApp code msg = throwIO (AppError code msg)

-- | リポジトリメソッドの境界。投げられた AppError を Left に畳む。
-- AppError 以外の例外はそのまま伝播し、HTTP 層で E999 になる。
tryApp :: IO a -> IO (Either AppError a)
tryApp = try

-- | 読み取った行を VO へ持ち上げる。DB の値がドメイン不変条件に反する
-- (通常は起きない) 場合はサーバエラー (E999) として扱う。
hydrate :: Either AppError a -> IO a
hydrate = either (\(AppError _ msg) -> throwApp Unexpected ("corrupt row: " <> msg)) pure

-- | 既存 HandlesQueryException の移植。MySQL のエラーコードを AppError に写す。
handleQueryException :: Text -> IO a -> IO a
handleQueryException message action = action `catch` \(ERRException err) ->
  case errCode err of
    1062 -> throwApp DuplicateEntry ("Duplicate entry. " <> message)
    1451 -> throwApp ChildRecordExists ("Child record exists. " <> message)
    1452 -> throwApp ParentRecordNotFound ("Parent record not found. " <> message)
    _ -> throwIO (ERRException err)

--------------------------------------------------------------------------------
-- 値変換ヘルパ
--------------------------------------------------------------------------------

valInt :: MySQLValue -> Int
valInt = \case
  MySQLInt8 v -> fromIntegral v
  MySQLInt8U v -> fromIntegral v
  MySQLInt16 v -> fromIntegral v
  MySQLInt16U v -> fromIntegral v
  MySQLInt32 v -> fromIntegral v
  MySQLInt32U v -> fromIntegral v
  MySQLInt64 v -> fromIntegral v
  MySQLInt64U v -> fromIntegral v
  MySQLDecimal v -> round v
  MySQLYear v -> fromIntegral v
  other -> error ("valInt: unexpected value " <> show other)

valIntMaybe :: MySQLValue -> Maybe Int
valIntMaybe MySQLNull = Nothing
valIntMaybe v = Just (valInt v)

valText :: MySQLValue -> Text
valText = \case
  MySQLText t -> t
  MySQLBytes b -> T.pack (show b)
  MySQLNull -> ""
  other -> T.pack (show other)

-- | DATE カラムを 'YYYY-MM-DD' 文字列にする (既存 Date VO の value() 相当)。
valDateText :: MySQLValue -> Text
valDateText = \case
  MySQLDate d -> T.pack (formatTime defaultTimeLocale "%Y-%m-%d" (d :: Day))
  MySQLText t -> t
  other -> T.pack (show other)

--------------------------------------------------------------------------------
-- パラメータ生成ヘルパ
--------------------------------------------------------------------------------

pInt :: Int -> MySQLValue
pInt = MySQLInt64 . fromIntegral

pText :: Text -> MySQLValue
pText = MySQLText

pDate :: Text -> MySQLValue
pDate = MySQLText

pNull :: MySQLValue
pNull = MySQLNull
