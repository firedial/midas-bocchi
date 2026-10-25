-- | HTTP 表現へのエラー整形。business のエラーコード (domain) を HTTP ステータスと
-- JSON ボディに写す。HTTP に固有の対応 (400/404/409/500) はここ (adapter) に閉じる。
module Adapter.Http.Error
  ( AppM
  , httpStatus
  , toServerError
  , jsonResponse
  , reasonPhrase
  ) where

import Control.Monad.Except (ExceptT)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)

import Network.HTTP.Types (hContentType)
import Network.HTTP.Types.Status (mkStatus)
import Network.Wai (Response, responseLBS)
import Servant (ServerError (..))

import Domain.Error (AppError (..), ErrorCode (..), codeValue)

-- | HTTP ハンドラの内部で使う。業務エラーは Left AppError で運ぶ。
type AppM = ExceptT AppError IO

httpStatus :: ErrorCode -> Int
httpStatus = \case
  Unauthorized -> 401
  RecordNotFound -> 404
  ParentRecordNotFound -> 409
  ChildRecordExists -> 409
  DuplicateEntry -> 409
  PageNotFound -> 404
  UnexpectedAttributeName -> 500
  UnexpectedAmount -> 500
  UnexpectedDifferenceIdMove -> 500
  UnexpectedNullRead -> 500
  Unexpected -> 500
  _ -> 400

toServerError :: AppError -> ServerError
toServerError (AppError code msg) = mkErr (httpStatus code) (codeValue code) msg

mkErr :: Int -> Text -> Text -> ServerError
mkErr status code msg = ServerError
  { errHTTPCode = status
  , errReasonPhrase = reasonPhrase status
  , errBody = encode (object ["code" .= code, "message" .= msg])
  , errHeaders = [(hContentType, "application/json")]
  }

-- | ミドルウェア / フォールバックで使う wai レスポンス。
jsonResponse :: Int -> Text -> Text -> Response
jsonResponse status code msg =
  responseLBS
    (mkStatus status (BC.pack (reasonPhrase status)))
    [(hContentType, "application/json")]
    (encode (object ["code" .= code, "message" .= msg]))

reasonPhrase :: Int -> String
reasonPhrase = \case
  400 -> "Bad Request"
  401 -> "Unauthorized"
  404 -> "Not Found"
  409 -> "Conflict"
  500 -> "Internal Server Error"
  _ -> ""
