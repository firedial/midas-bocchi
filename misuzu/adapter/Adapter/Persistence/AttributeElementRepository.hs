module Adapter.Persistence.AttributeElementRepository
  ( newAttributeElementRepository
  ) where

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Database.MySQL.Base (MySQLConn, MySQLValue)

import Adapter.Persistence.Db
import Domain.Attribute (Attribute (..))
import Domain.Entities (Element (..), ElementInput (..))
import Domain.Repository.AttributeElement (AttributeElementRepository (..))
import Domain.ValueObjects
  ( Description
  , ElementName
  , Priority
  , mkDescription
  , mkElementName
  , mkId
  , mkPriority
  , unDescription
  , unElementName
  , unId
  , unPriority
  )

newAttributeElementRepository :: Db -> AttributeElementRepository IO
newAttributeElementRepository db = AttributeElementRepository
  { listElements = \attr ->
      withConn db $ \c -> withTx c $ selectElementsImpl c attr Nothing
  , findElement = \attr theId ->
      withConn db $ \c -> withTx c $ listToMaybe <$> selectElementsImpl c attr (Just theId)
  , createElement = \attr ElementInput {..} ->
      tryApp $ withConn db $ \c -> withTx c $ insertElementImpl c attr eiName eiDescription eiPriority (unId eiCategoryId)
  , updateElement = \attr theId ElementInput {..} ->
      tryApp $ withConn db $ \c -> withTx c $ updateElementImpl c attr theId eiName eiDescription eiPriority (unId eiCategoryId)
  }

tableFor :: Attribute -> Text
tableFor Kind = "m_kind_element"
tableFor Purpose = "m_purpose_element"
tableFor Place = "m_place_element"

parseElement :: [MySQLValue] -> IO Element
parseElement [i, n, d, p, c] = do
  name <- hydrate (mkElementName (valText n))
  desc <- hydrate (mkDescription (valText d))
  priority <- hydrate (mkPriority (valInt p))
  pure Element
    { elId = mkId (valInt i)
    , elName = name
    , elDescription = desc
    , elPriority = priority
    , elCategoryId = mkId (valInt c)
    }
parseElement row = error ("parseElement: unexpected columns " <> show (length row))

selectElementsImpl :: MySQLConn -> Attribute -> Maybe Int -> IO [Element]
selectElementsImpl conn attr mId = do
  let table = tableFor attr
      whereClause = maybe "" (const " WHERE id = ?") mId
      sql = "SELECT id, name, description, priority, category_id FROM " <> table
              <> whereClause <> " ORDER BY priority DESC, id ASC"
      params = maybe [] (\i -> [pInt i]) mId
  rows <- runQuery conn sql params
  mapM parseElement rows

insertElementImpl :: MySQLConn -> Attribute -> ElementName -> Description -> Priority -> Int -> IO Element
insertElementImpl conn attr name desc priority categoryId =
  handleQueryException ("Insert " <> unElementName name <> " element error.") $ do
    let table = tableFor attr
        sql = "INSERT INTO " <> table
                <> " (name, description, priority, category_id) VALUES (?, ?, ?, ?)"
    ok <- runExec conn sql [pText (unElementName name), pText (unDescription desc), pInt (unPriority priority), pInt categoryId]
    pure (Element (mkId (lastInsertId ok)) name desc priority (mkId categoryId))

updateElementImpl :: MySQLConn -> Attribute -> Int -> ElementName -> Description -> Priority -> Int -> IO Element
updateElementImpl conn attr theId name desc priority categoryId =
  handleQueryException ("Update " <> unElementName name <> " element error.") $ do
    let table = tableFor attr
        sql = "UPDATE " <> table
                <> " SET name = ?, description = ?, priority = ?, category_id = ? WHERE id = ?"
    _ <- runExec conn sql [pText (unElementName name), pText (unDescription desc), pInt (unPriority priority), pInt categoryId, pInt theId]
    pure (Element (mkId theId) name desc priority (mkId categoryId))
