module Domain.Error
  ( ErrorCode (..)
  , codeValue
  , AppError (..)
  ) where

import Control.Exception (Exception)
import Data.Text (Text)

-- | 既存 haruhi の App\Exceptions\ErrorCode を移植。
data ErrorCode
  = InvalidType -- E101
  | InvalidRange -- E102
  | InvalidFormat -- E103
  | InvalidLength -- E105
  | InvalidValue -- E106
  | MoveSameId -- E107
  | UsingMoveId -- E108
  | MissingRequired -- E109
  | Unauthorized -- E201
  | RecordNotFound -- E301
  | ParentRecordNotFound -- E302
  | ChildRecordExists -- E303
  | DuplicateEntry -- E304
  | PageNotFound -- E305
  | UnexpectedAttributeName -- E901
  | UnexpectedAmount -- E902
  | UnexpectedDifferenceIdMove -- E903
  | UnexpectedNullRead -- E904
  | Unexpected -- E999
  deriving (Eq, Show)

codeValue :: ErrorCode -> Text
codeValue = \case
  InvalidType -> "E101"
  InvalidRange -> "E102"
  InvalidFormat -> "E103"
  InvalidLength -> "E105"
  InvalidValue -> "E106"
  MoveSameId -> "E107"
  UsingMoveId -> "E108"
  MissingRequired -> "E109"
  Unauthorized -> "E201"
  RecordNotFound -> "E301"
  ParentRecordNotFound -> "E302"
  ChildRecordExists -> "E303"
  DuplicateEntry -> "E304"
  PageNotFound -> "E305"
  UnexpectedAttributeName -> "E901"
  UnexpectedAmount -> "E902"
  UnexpectedDifferenceIdMove -> "E903"
  UnexpectedNullRead -> "E904"
  Unexpected -> "E999"

-- | 業務エラーを表す純粋なデータ。
-- 例外としても扱えるようにしておき、永続化層 (adapter) が MySQL 例外を
-- AppError に変換して境界で捕捉する用途に使う。domain/usecase では値として運ぶ。
data AppError = AppError ErrorCode Text
  deriving (Show)

instance Exception AppError
