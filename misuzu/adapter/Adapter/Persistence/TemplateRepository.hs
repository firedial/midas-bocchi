module Adapter.Persistence.TemplateRepository
  ( newTemplateRepository
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Database.MySQL.Base (MySQLConn, MySQLValue)

import Adapter.Persistence.Db
import Domain.Entities (Template (..), TemplateDetail (..), TemplateInput (..))
import Domain.Error (ErrorCode (..))
import Domain.Repository.Template (TemplateRepository (..))
import Domain.ValueObjects
  ( TemplateName
  , mkAmount
  , mkId
  , mkItem
  , mkTemplateName
  , unAmount
  , unId
  , unItem
  , unTemplateName
  )

newTemplateRepository :: Db -> TemplateRepository IO
newTemplateRepository db = TemplateRepository
  { listTemplates =
      withConn db $ \c -> withTx c $ selectTemplatesImpl c
  , findTemplate = \theId ->
      withConn db $ \c -> withTx c $ selectTemplateImpl c theId
  , createTemplate = \TemplateInput {..} ->
      tryApp $ withConn db $ \c -> withTx c $ insertTemplateImpl c tiName tiDetails
  , updateTemplate = \theId TemplateInput {..} ->
      tryApp $ withConn db $ \c -> withTx c $ updateTemplateImpl c theId tiName tiDetails
  , deleteTemplate = \theId ->
      tryApp $ withConn db $ \c -> withTx c $ deleteTemplateImpl c theId
  }

detailColumns :: Text
detailColumns = T.intercalate ", "
  [ "seq", "type", "amount", "item"
  , "kind_element_id", "purpose_element_id", "place_element_id"
  , "move_before_purpose_id", "move_after_purpose_id"
  , "move_before_place_id", "move_after_place_id"
  ]

parseTemplateRow :: [MySQLValue] -> (Int, Text)
parseTemplateRow [i, n] = (valInt i, valText n)
parseTemplateRow row = error ("parseTemplateRow: unexpected columns " <> show (length row))

parseDetail :: [MySQLValue] -> IO TemplateDetail
parseDetail [s, t, a, it, k, pu, pl, mbp, map', mbpl, mapl] = do
  item <- hydrate (mkItem (valText it))
  pure TemplateDetail
    { tdSeq = valInt s
    , tdType = valInt t
    , tdAmount = mkAmount (valInt a)
    , tdItem = item
    , tdKindElementId = mkId <$> valIntMaybe k
    , tdPurposeElementId = mkId <$> valIntMaybe pu
    , tdPlaceElementId = mkId <$> valIntMaybe pl
    , tdMoveBeforePurposeId = mkId <$> valIntMaybe mbp
    , tdMoveAfterPurposeId = mkId <$> valIntMaybe map'
    , tdMoveBeforePlaceId = mkId <$> valIntMaybe mbpl
    , tdMoveAfterPlaceId = mkId <$> valIntMaybe mapl
    }
parseDetail row = error ("parseDetail: unexpected columns " <> show (length row))

selectTemplatesImpl :: MySQLConn -> IO [Template]
selectTemplatesImpl conn = do
  rows <- runQuery_ conn "SELECT id, name FROM m_template ORDER BY id ASC"
  pure [Template (mkId i) (mkTemplateName n) [] | row <- rows, let (i, n) = parseTemplateRow row]

selectTemplateImpl :: MySQLConn -> Int -> IO (Maybe Template)
selectTemplateImpl conn theId = do
  rows <- runQuery conn "SELECT id, name FROM m_template WHERE id = ?" [pInt theId]
  case rows of
    [] -> pure Nothing
    (r : _) -> do
      let (i, n) = parseTemplateRow r
      detailRows <- runQuery conn
        ("SELECT " <> detailColumns <> " FROM m_template_detail WHERE template_id = ? ORDER BY seq ASC")
        [pInt theId]
      details <- mapM parseDetail detailRows
      pure (Just (Template (mkId i) (mkTemplateName n) details))

insertDetails :: MySQLConn -> Int -> [TemplateDetail] -> IO ()
insertDetails conn tid details =
  handleQueryException "Insert template detail error." $
    mapM_ (insertOne conn tid) details

insertOne :: MySQLConn -> Int -> TemplateDetail -> IO ()
insertOne conn tid d = do
  let sql = "INSERT INTO m_template_detail (template_id, " <> detailColumns
              <> ") VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
      mId = maybe pNull (pInt . unId)
      params =
        [ pInt tid, pInt (tdSeq d), pInt (tdType d), pInt (unAmount (tdAmount d)), pText (unItem (tdItem d))
        , mId (tdKindElementId d), mId (tdPurposeElementId d), mId (tdPlaceElementId d)
        , mId (tdMoveBeforePurposeId d), mId (tdMoveAfterPurposeId d)
        , mId (tdMoveBeforePlaceId d), mId (tdMoveAfterPlaceId d)
        ]
  _ <- runExec conn sql params
  pure ()

insertTemplateImpl :: MySQLConn -> TemplateName -> [TemplateDetail] -> IO Template
insertTemplateImpl conn name details = do
  ok <- runExec conn "INSERT INTO m_template (name) VALUES (?)" [pText (unTemplateName name)]
  let tid = lastInsertId ok
  insertDetails conn tid details
  pure (Template (mkId tid) name details)

updateTemplateImpl :: MySQLConn -> Int -> TemplateName -> [TemplateDetail] -> IO Template
updateTemplateImpl conn theId name details = do
  _ <- runExec conn "UPDATE m_template SET name = ? WHERE id = ?" [pText (unTemplateName name), pInt theId]
  _ <- runExec conn "DELETE FROM m_template_detail WHERE template_id = ?" [pInt theId]
  insertDetails conn theId details
  pure (Template (mkId theId) name details)

deleteTemplateImpl :: MySQLConn -> Int -> IO Template
deleteTemplateImpl conn theId = do
  rows <- runQuery conn "SELECT id, name FROM m_template WHERE id = ?" [pInt theId]
  case rows of
    [] -> throwApp RecordNotFound "Not found template."
    (r : _) -> do
      let (i, n) = parseTemplateRow r
      _ <- runExec conn "DELETE FROM m_template WHERE id = ?" [pInt theId]
      pure (Template (mkId i) (mkTemplateName n) [])
