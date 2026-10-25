module Adapter.Http.Api
  ( API
  , BalancesAPI
  , MovesAPI
  , ElementsAPI
  , CategoriesAPI
  , TemplatesAPI
  , api
  ) where

import Data.Aeson (Value)
import Data.Text (Text)
import Servant

type BalancesAPI = "balances" :>
  ( QueryParam "limit" Text :> QueryParam "orderby" Text :> Get '[JSON] Value
  :<|> ReqBody '[JSON] Value :> Post '[JSON] Value
  :<|> Capture "id" Int :> Get '[JSON] Value
  :<|> Capture "id" Int :> ReqBody '[JSON] Value :> Put '[JSON] Value
  :<|> Capture "id" Int :> Delete '[JSON] Value
  )

type MovesAPI = "moves" :> Capture "attr" Text :>
  ( QueryParam "limit" Text :> Get '[JSON] Value
  :<|> ReqBody '[JSON] Value :> Post '[JSON] Value
  :<|> Capture "id" Int :> Get '[JSON] Value
  :<|> Capture "id" Int :> ReqBody '[JSON] Value :> Put '[JSON] Value
  :<|> Capture "id" Int :> Delete '[JSON] Value
  )

type ElementsAPI = "attribute_elements" :> Capture "attr" Text :>
  ( Get '[JSON] Value
  :<|> ReqBody '[JSON] Value :> Post '[JSON] Value
  :<|> Capture "id" Int :> Get '[JSON] Value
  :<|> Capture "id" Int :> ReqBody '[JSON] Value :> Put '[JSON] Value
  )

type CategoriesAPI = "attribute_categories" :> Capture "attr" Text :>
  ( Get '[JSON] Value
  :<|> ReqBody '[JSON] Value :> Post '[JSON] Value
  :<|> Capture "id" Int :> Get '[JSON] Value
  :<|> Capture "id" Int :> ReqBody '[JSON] Value :> Put '[JSON] Value
  )

type TemplatesAPI = "templates" :>
  ( Get '[JSON] Value
  :<|> ReqBody '[JSON] Value :> Post '[JSON] Value
  :<|> Capture "id" Int :> Get '[JSON] Value
  :<|> Capture "id" Int :> ReqBody '[JSON] Value :> Put '[JSON] Value
  :<|> Capture "id" Int :> Delete '[JSON] Value
  )

type API = "api" :>
  ( BalancesAPI
  :<|> MovesAPI
  :<|> ElementsAPI
  :<|> CategoriesAPI
  :<|> TemplatesAPI
  :<|> Raw
  )

api :: Proxy API
api = Proxy
