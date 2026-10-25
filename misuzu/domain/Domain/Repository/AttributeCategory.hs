module Domain.Repository.AttributeCategory
  ( AttributeCategoryRepository (..)
  ) where

import Domain.Attribute (Attribute)
import Domain.Entities (Category, CategoryInput)
import Domain.Error (AppError)

data AttributeCategoryRepository m = AttributeCategoryRepository
  { listCategories :: Attribute -> m [Category]
  , findCategory :: Attribute -> Int -> m (Maybe Category)
  , createCategory :: Attribute -> CategoryInput -> m (Either AppError Category)
  , updateCategory :: Attribute -> Int -> CategoryInput -> m (Either AppError Category)
  }
