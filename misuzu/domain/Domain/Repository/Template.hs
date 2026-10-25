module Domain.Repository.Template
  ( TemplateRepository (..)
  ) where

import Domain.Entities (Template, TemplateInput)
import Domain.Error (AppError)

data TemplateRepository m = TemplateRepository
  { listTemplates :: m [Template]
    -- ^ 明細なしのサマリ一覧。
  , findTemplate :: Int -> m (Maybe Template)
    -- ^ 明細込みの 1 件取得。
  , createTemplate :: TemplateInput -> m (Either AppError Template)
  , updateTemplate :: Int -> TemplateInput -> m (Either AppError Template)
  , deleteTemplate :: Int -> m (Either AppError Template)
  }
