-- | 値オブジェクト。コンストラクタを隠し、mk* を通らないと値を作れない。
-- 「その型の値が存在する = 検証済み」を型で保証する。un* で素の値を取り出す。
--
-- 不変条件を持つもの (Item/Description/名前/Priority/Date) は mk* が Either を返して検証する。
-- amount と各 id、template name は不変条件が文脈依存 (収支は amount≠0 / 移動は≥1、id は
-- 使用時に≠1、name は空の扱いが分岐) のため mk* は素通しの薄いラッパーとし、実際のチェックは
-- usecase/adapter 側 (checkAmountNonZero/filledId/isMoveId 等) に残す。
module Domain.ValueObjects
  ( -- Amount
    Amount
  , mkAmount
  , unAmount
  , checkAmountNonZero
    -- Item
  , Item
  , mkItem
  , unItem
    -- Description
  , Description
  , mkDescription
  , unDescription
    -- Name
  , ElementName
  , mkElementName
  , unElementName
  , CategoryName
  , mkCategoryName
  , unCategoryName
  , TemplateName
  , mkTemplateName
  , unTemplateName
    -- Priority
  , Priority
  , mkPriority
  , unPriority
    -- Date
  , Date
  , mkDate
  , dateText
    -- Id
  , Id
  , mkId
  , unId
  , moveId
  , filledId
  , isMoveId
  ) where

import Data.Char (isAscii, isLower, isUpper)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorianValid)
import Text.Printf (printf)
import Text.Read (readMaybe)

import Domain.Error (AppError (..), ErrorCode (..))

-- | 検証失敗を表す短縮子。domain は純粋なので Either で返す。
invalid :: ErrorCode -> Text -> Either AppError a
invalid code msg = Left (AppError code msg)

--------------------------------------------------------------------------------
-- Amount (薄いラッパー。金額の可否は文脈依存なので checkAmountNonZero 等で別途チェック)
--------------------------------------------------------------------------------

newtype Amount = Amount Int
  deriving (Eq, Show)

mkAmount :: Int -> Amount
mkAmount = Amount

unAmount :: Amount -> Int
unAmount (Amount n) = n

-- | 既存 Controller の「amount が 0 なら E102」を移植。
checkAmountNonZero :: Int -> Either AppError ()
checkAmountNonZero a
  | a == 0 = invalid InvalidRange "Amount is zero."
  | otherwise = Right ()

--------------------------------------------------------------------------------
-- Item / Description (長さ)
--------------------------------------------------------------------------------

checkLength :: Text -> Int -> Text -> Either AppError ()
checkLength label maxLen v
  | T.length v == 0 = invalid InvalidLength (label <> " length is empty.")
  | T.length v > maxLen = invalid InvalidLength (label <> " length is over.")
  | otherwise = Right ()

-- | Item VO (最大 50 文字)。
newtype Item = Item Text
  deriving (Eq, Show)

mkItem :: Text -> Either AppError Item
mkItem v = Item v <$ checkLength "Item" 50 v

unItem :: Item -> Text
unItem (Item v) = v

-- | Description VO (最大 20 文字)。
newtype Description = Description Text
  deriving (Eq, Show)

mkDescription :: Text -> Either AppError Description
mkDescription v = Description v <$ checkLength "Description" 20 v

unDescription :: Description -> Text
unDescription (Description v) = v

--------------------------------------------------------------------------------
-- Name (長さ + 書式)
--------------------------------------------------------------------------------

-- | AttributeElementName VO: 1..20 文字 & /^[a-z]\w*$/。
newtype ElementName = ElementName Text
  deriving (Eq, Show)

mkElementName :: Text -> Either AppError ElementName
mkElementName v = do
  checkLength "Name" 20 v
  if matchesFirstRest isLower v
    then Right (ElementName v)
    else invalid InvalidFormat "Name contains invalid characters."

unElementName :: ElementName -> Text
unElementName (ElementName v) = v

-- | AttributeCategoryName VO: 1..20 文字 & /^[A-Z]\w*$/。
newtype CategoryName = CategoryName Text
  deriving (Eq, Show)

mkCategoryName :: Text -> Either AppError CategoryName
mkCategoryName v = do
  checkLength "Name" 20 v
  if matchesFirstRest isUpper v
    then Right (CategoryName v)
    else invalid InvalidFormat "Name contains invalid characters."

unCategoryName :: CategoryName -> Text
unCategoryName (CategoryName v) = v

-- | TemplateName VO (薄いラッパー)。空(E109)/長さ(E105) の扱いが分岐するため検証は adapter 側。
newtype TemplateName = TemplateName Text
  deriving (Eq, Show)

mkTemplateName :: Text -> TemplateName
mkTemplateName = TemplateName

unTemplateName :: TemplateName -> Text
unTemplateName (TemplateName v) = v

-- | 先頭文字が pred を満たし、以降が \w (=[A-Za-z0-9_]) であること。
matchesFirstRest :: (Char -> Bool) -> Text -> Bool
matchesFirstRest firstPred t = case T.uncons t of
  Nothing -> False
  Just (c, r) -> firstPred c && isAscii c && T.all isWordChar r
  where
    isWordChar ch = isAscii ch && (ch == '_' || charIsAlphaNum ch)
    charIsAlphaNum ch = (ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')

--------------------------------------------------------------------------------
-- Priority
--------------------------------------------------------------------------------

-- | Priority VO (0..100)。
newtype Priority = Priority Int
  deriving (Eq, Show)

mkPriority :: Int -> Either AppError Priority
mkPriority p
  | p < 0 || p > 100 = invalid InvalidRange "Priority is wrong."
  | otherwise = Right (Priority p)

unPriority :: Priority -> Int
unPriority (Priority p) = p

--------------------------------------------------------------------------------
-- Date
--------------------------------------------------------------------------------

-- | Date VO。'-' 区切り 3 要素、実在する日付のみ。value() は正規形 'YYYY-MM-DD'。
data Date = Date Int Int Int
  deriving (Eq, Show)

-- | 既存 Date VO を移植。'-' 区切り 3 要素、実在する日付でなければ E103。
mkDate :: Text -> Either AppError Date
mkDate raw = case map (readMaybe . T.unpack) (T.splitOn "-" raw) of
  [Just y, Just m, Just d]
    | validGregorian y m d -> Right (Date y m d)
  _ -> invalid InvalidFormat "Date is wrong."
  where
    validGregorian y m d =
      case fromGregorianValid (fromIntegral y) m d of
        Just _ -> True
        Nothing -> False

-- | sprintf('%d-%02d-%02d', y, m, d) 相当。
dateText :: Date -> Text
dateText (Date y m d) = T.pack (printf "%d-%02d-%02d" y m d)

--------------------------------------------------------------------------------
-- Id (薄いラッパー。使用時ルール filledId/isMoveId は素の Int に対して別途チェック)
--------------------------------------------------------------------------------

newtype Id = Id Int
  deriving (Eq, Show)

mkId :: Int -> Id
mkId = Id

unId :: Id -> Int
unId (Id n) = n

moveId :: Int
moveId = 1

isMoveId :: Int -> Bool
isMoveId = (== moveId)

-- | 既存 Id::filledId。0 は空 ID とみなし E106。
filledId :: Int -> Either AppError Int
filledId 0 = invalid InvalidValue "Empty id is used."
filledId n = Right n
