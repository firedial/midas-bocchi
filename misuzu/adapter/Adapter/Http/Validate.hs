-- | リクエスト表現 (JSON / クエリ) の検証。JSON の presence/型 (E109/E101) と、
-- domain の smart constructor を呼び出して検証済みのコマンドを組み立てるための補助。
-- すべて純粋な Either で返し、失敗コードの順序は既存ハンドラのまま維持する。
module Adapter.Http.Validate
  ( -- JSON 抽出
    asObject
  , lookupField
  , intOfJson
  , reqInt
  , requiredInt
  , reqStr
  , requiredStr
  , optNullableInt
    -- クエリ・範囲・業務補助
  , parseLimit
  , parseOrderBy
  , checkGroupMin
  , requireMin1
  , checkElementNotMove
  , checkMoveElements
  ) where

import Control.Monad (when)
import Data.Aeson (Object, Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Scientific as Sci
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

import Domain.Error (AppError (..), ErrorCode (..))
import Domain.ValueObjects (isMoveId)

invalid :: ErrorCode -> Text -> Either AppError a
invalid code msg = Left (AppError code msg)

--------------------------------------------------------------------------------
-- JSON 抽出 (既存 Json モジュール移植)
--------------------------------------------------------------------------------

-- | リクエストボディを JSON オブジェクトとして扱う。オブジェクトでなければ空扱い。
asObject :: Value -> Object
asObject (Object o) = o
asObject _ = KM.empty

lookupField :: Text -> Object -> Maybe Value
lookupField k = KM.lookup (Key.fromText k)

-- | 既存の ['present', StrictInteger] 相当。キー無し → E109、null/文字列/非整数 → E101。
reqInt :: Object -> Text -> Either AppError Int
reqInt o field = case lookupField field o of
  Nothing -> invalid MissingRequired (field <> " is required")
  Just v -> intOfValue field v

-- | 既存の 'present|string' 相当。キー無し → E109、非文字列(null含む) → E101。
reqStr :: Object -> Text -> Either AppError Text
reqStr o field = case lookupField field o of
  Nothing -> invalid MissingRequired (field <> " is required")
  Just (String s) -> Right s
  Just _ -> invalid InvalidType (field <> " must be a string type")

-- | 既存の ['required', StrictInteger] 相当。キー無し/null → E109、非整数 → E101。
requiredInt :: Object -> Text -> Either AppError Int
requiredInt o field = case lookupField field o of
  Nothing -> invalid MissingRequired (field <> " is required")
  Just Null -> invalid MissingRequired (field <> " is required")
  Just v -> intOfValue field v

-- | JSON 値が整数なら Just、それ以外は Nothing。
intOfJson :: Value -> Maybe Int
intOfJson (Number n) = Sci.toBoundedInteger n
intOfJson _ = Nothing

-- | 既存の 'required|string' 相当。キー無し/null/空文字 → E109、非文字列 → E101。
requiredStr :: Object -> Text -> Either AppError Text
requiredStr o field = case lookupField field o of
  Nothing -> invalid MissingRequired (field <> " is required")
  Just Null -> invalid MissingRequired (field <> " is required")
  Just (String "") -> invalid MissingRequired (field <> " is required")
  Just (String s) -> Right s
  Just _ -> invalid InvalidType (field <> " must be a string type")

-- | 既存の ['nullable', StrictInteger(nullable:true)] 相当 (POST group_id)。
-- キー無し/null → Nothing、非整数(文字列含む) → E101。
optNullableInt :: Object -> Text -> Either AppError (Maybe Int)
optNullableInt o field = case lookupField field o of
  Nothing -> Right Nothing
  Just Null -> Right Nothing
  Just v -> Just <$> intOfValue field v

intOfValue :: Text -> Value -> Either AppError Int
intOfValue field = \case
  Number n -> case Sci.toBoundedInteger n :: Maybe Int of
    Just i -> Right i
    Nothing -> invalid InvalidType (field <> " must be an integer type")
  _ -> invalid InvalidType (field <> " must be an integer type")

--------------------------------------------------------------------------------
-- クエリ・範囲・業務補助 (既存 Http.Validate / ハンドラ移植)
--------------------------------------------------------------------------------

-- | index の limit バリデーション (['int','min:1'])。
parseLimit :: Maybe Text -> Either AppError (Maybe Int)
parseLimit Nothing = Right Nothing
parseLimit (Just t) = case readMaybe (T.unpack t) :: Maybe Int of
  Nothing -> invalid InvalidType "limit must be a int type"
  Just n
    | n < 1 -> invalid InvalidRange "limit must be at least 1"
    | otherwise -> Right (Just n)

-- | index の orderby バリデーション (['string','in:desc'])。
parseOrderBy :: Maybe Text -> Either AppError Bool
parseOrderBy Nothing = Right False
parseOrderBy (Just "desc") = Right True
parseOrderBy (Just _) = invalid InvalidFormat "orderby is not a valid value"

-- | nullable な group_id の min:1 チェック。
checkGroupMin :: Maybe Int -> Either AppError ()
checkGroupMin (Just g) | g < 1 = invalid InvalidRange "group_id must be at least 1"
checkGroupMin _ = Right ()

-- | 必須整数の min:1 チェック (E102)。
requireMin1 :: Text -> Int -> Either AppError ()
requireMin1 field n
  | n < 1 = invalid InvalidRange (field <> " must be at least 1")
  | otherwise = Right ()

-- | 収支の kind/purpose/place が move id でないこと (E108)。
checkElementNotMove :: Int -> Int -> Int -> Either AppError ()
checkElementNotMove kind purpose place = do
  when (isMoveId kind) (invalid UsingMoveId "Kind element id is move id.")
  when (isMoveId purpose) (invalid UsingMoveId "Purpose element id is move id.")
  when (isMoveId place) (invalid UsingMoveId "Place element id is move id.")

-- | 移動の before/after が同一でなく (E107)、move id でないこと (E108)。
checkMoveElements :: Int -> Int -> Either AppError ()
checkMoveElements before after = do
  when (before == after) (invalid MoveSameId "Before id and after id is the same.")
  when (isMoveId before || isMoveId after) (invalid UsingMoveId "Before id or after id is move id.")
