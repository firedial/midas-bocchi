module Adapter.Persistence.AttributeCategoryRepository
  ( newAttributeCategoryRepository
  ) where

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Database.MySQL.Base (MySQLConn, MySQLValue)

import Adapter.Persistence.Db
import Domain.Attribute (Attribute (..))
import Domain.Entities (Category (..), CategoryInput (..))
import Domain.Repository.AttributeCategory (AttributeCategoryRepository (..))
import Domain.ValueObjects
  ( CategoryName
  , Description
  , mkCategoryName
  , mkDescription
  , mkId
  , unCategoryName
  , unDescription
  )

newAttributeCategoryRepository :: Db -> AttributeCategoryRepository IO
newAttributeCategoryRepository db = AttributeCategoryRepository
  { listCategories = \attr ->
      withConn db $ \c -> withTx c $ selectCategoriesImpl c attr Nothing
  , findCategory = \attr theId ->
      withConn db $ \c -> withTx c $ listToMaybe <$> selectCategoriesImpl c attr (Just theId)
  , createCategory = \attr CategoryInput {..} ->
      tryApp $ withConn db $ \c -> withTx c $ insertCategoryImpl c attr ciName ciDescription
  , updateCategory = \attr theId CategoryInput {..} ->
      tryApp $ withConn db $ \c -> withTx c $ updateCategoryImpl c attr theId ciName ciDescription
  }

tableFor :: Attribute -> Text
tableFor Kind = "m_kind_category"
tableFor Purpose = "m_purpose_category"
tableFor Place = "m_place_category"

parseCategory :: [MySQLValue] -> IO Category
parseCategory [i, n, d] = do
  name <- hydrate (mkCategoryName (valText n))
  desc <- hydrate (mkDescription (valText d))
  pure Category
    { catId = mkId (valInt i)
    , catName = name
    , catDescription = desc
    }
parseCategory row = error ("parseCategory: unexpected columns " <> show (length row))

selectCategoriesImpl :: MySQLConn -> Attribute -> Maybe Int -> IO [Category]
selectCategoriesImpl conn attr mId = do
  let table = tableFor attr
      whereClause = maybe "" (const " WHERE id = ?") mId
      sql = "SELECT id, name, description FROM " <> table <> whereClause <> " ORDER BY id ASC"
      params = maybe [] (\i -> [pInt i]) mId
  rows <- runQuery conn sql params
  mapM parseCategory rows

insertCategoryImpl :: MySQLConn -> Attribute -> CategoryName -> Description -> IO Category
insertCategoryImpl conn attr name desc =
  handleQueryException ("Insert " <> unCategoryName name <> " category error.") $ do
    let sql = "INSERT INTO " <> tableFor attr <> " (name, description) VALUES (?, ?)"
    ok <- runExec conn sql [pText (unCategoryName name), pText (unDescription desc)]
    pure (Category (mkId (lastInsertId ok)) name desc)

updateCategoryImpl :: MySQLConn -> Attribute -> Int -> CategoryName -> Description -> IO Category
updateCategoryImpl conn attr theId name desc =
  handleQueryException ("Update " <> unCategoryName name <> " category error.") $ do
    let sql = "UPDATE " <> tableFor attr <> " SET name = ?, description = ? WHERE id = ?"
    _ <- runExec conn sql [pText (unCategoryName name), pText (unDescription desc), pInt theId]
    pure (Category (mkId theId) name desc)
