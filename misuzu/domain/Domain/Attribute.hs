module Domain.Attribute
  ( Attribute (..)
  , isKind
  , isPurpose
  , isPlace
  , parseMoveAttr
  , parseElementAttr
  , parseCategoryAttr
  ) where

import Data.Text (Text)

import Domain.Error (AppError (..), ErrorCode (..))

data Attribute = Kind | Purpose | Place
  deriving (Eq, Show)

isKind, isPurpose, isPlace :: Attribute -> Bool
isKind = (== Kind)
isPurpose = (== Purpose)
isPlace = (== Place)

-- 既存ルートは whereIn で属性名を制限しており、範囲外は fallback (E305) になる。
-- misuzu では Capture が任意文字列を拾うため、ここで同じ 404 に落とす。

notFound :: Either AppError a
notFound = Left (AppError PageNotFound "Page not found.")

parseMoveAttr :: Text -> Either AppError Attribute
parseMoveAttr = \case
  "purposes" -> Right Purpose
  "places" -> Right Place
  _ -> notFound

parseElementAttr :: Text -> Either AppError Attribute
parseElementAttr = \case
  "kind_element" -> Right Kind
  "purpose_element" -> Right Purpose
  "place_element" -> Right Place
  _ -> notFound

parseCategoryAttr :: Text -> Either AppError Attribute
parseCategoryAttr = \case
  "kind_category" -> Right Kind
  "purpose_category" -> Right Purpose
  "place_category" -> Right Place
  _ -> notFound
