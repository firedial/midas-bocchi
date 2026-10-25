module Domain.Entities
  ( Balance (..)
  , Move (..)
  , Element (..)
  , Category (..)
  , Template (..)
  , TemplateDetail (..)
    -- 書き込み入力 (検証済みのコマンド)
  , BalanceInput (..)
  , MoveInput (..)
  , ElementInput (..)
  , CategoryInput (..)
  , TemplateInput (..)
  ) where

import Domain.ValueObjects
  ( Amount
  , CategoryName
  , Date
  , Description
  , ElementName
  , Id
  , Item
  , Priority
  , TemplateName
  )

-- | m_balance の 1 行 (収支)。group_id は解決済みの値を保持する。
-- *Desc は結合で得た表示用の非正規化フィールド (要素の description) で Maybe。
data Balance = Balance
  { balId :: Id
  , balAmount :: Amount
  , balItem :: Item
  , balKind :: Id
  , balPurpose :: Id
  , balPlace :: Id
  , balDate :: Date
  , balGroupId :: Id
  , balKindDesc :: Maybe Description
  , balPurposeDesc :: Maybe Description
  , balPlaceDesc :: Maybe Description
  }

-- | 移動 (m_balance の 2 行ペアを 1 件として見たもの)。
data Move = Move
  { mvId :: Id
  , mvAmount :: Amount
  , mvItem :: Item
  , mvBeforeId :: Id
  , mvAfterId :: Id
  , mvDate :: Date
  , mvGroupId :: Id
  , mvBeforeDesc :: Maybe Description
  , mvAfterDesc :: Maybe Description
  }

-- | m_{kind,purpose,place}_element の 1 行。
data Element = Element
  { elId :: Id
  , elName :: ElementName
  , elDescription :: Description
  , elPriority :: Priority
  , elCategoryId :: Id
  }

-- | m_{kind,purpose,place}_category の 1 行。
data Category = Category
  { catId :: Id
  , catName :: CategoryName
  , catDescription :: Description
  }

-- | m_template の 1 行 + 明細。
data Template = Template
  { tmplId :: Id
  , tmplName :: TemplateName
  , tmplDetails :: [TemplateDetail]
  }

-- | seq は行の並び順、type は明細種別 (1/2/3) の構造的な値なので素の Int のまま。
data TemplateDetail = TemplateDetail
  { tdSeq :: Int
  , tdType :: Int
  , tdAmount :: Amount
  , tdItem :: Item
  , tdKindElementId :: Maybe Id
  , tdPurposeElementId :: Maybe Id
  , tdPlaceElementId :: Maybe Id
  , tdMoveBeforePurposeId :: Maybe Id
  , tdMoveAfterPurposeId :: Maybe Id
  , tdMoveBeforePlaceId :: Maybe Id
  , tdMoveAfterPlaceId :: Maybe Id
  }

--------------------------------------------------------------------------------
-- 書き込み入力 (adapter が VO を組み立てる。group_id は insert 時 Nothing で自動採番)
--------------------------------------------------------------------------------

data BalanceInput = BalanceInput
  { biAmount :: Amount
  , biItem :: Item
  , biKind :: Id
  , biPurpose :: Id
  , biPlace :: Id
  , biDate :: Date
  , biGroupId :: Maybe Id
  }

data MoveInput = MoveInput
  { miAmount :: Amount
  , miItem :: Item
  , miBeforeId :: Id
  , miAfterId :: Id
  , miDate :: Date
  , miGroupId :: Maybe Id
  }

data ElementInput = ElementInput
  { eiName :: ElementName
  , eiDescription :: Description
  , eiPriority :: Priority
  , eiCategoryId :: Id
  }

data CategoryInput = CategoryInput
  { ciName :: CategoryName
  , ciDescription :: Description
  }

data TemplateInput = TemplateInput
  { tiName :: TemplateName
  , tiDetails :: [TemplateDetail]
  }
