module Domain.Repository.AttributeElement
  ( AttributeElementRepository (..)
  ) where

import Domain.Attribute (Attribute)
import Domain.Entities (Element, ElementInput)
import Domain.Error (AppError)

data AttributeElementRepository m = AttributeElementRepository
  { listElements :: Attribute -> m [Element]
  , findElement :: Attribute -> Int -> m (Maybe Element)
  , createElement :: Attribute -> ElementInput -> m (Either AppError Element)
  , updateElement :: Attribute -> Int -> ElementInput -> m (Either AppError Element)
  }
