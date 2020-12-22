{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

-- | Описание API
module Api where

import Servant.API
import Servant.Swagger.UI (SwaggerSchemaUI)
import Data.UUID (UUID)
import Data.Proxy (Proxy(..))

import Types

type Api
  = "StartGame" :> Post '[JSON] GameState
  :<|> "GetGameState" :> Header "Uuid" UUID :> Get '[JSON] GameState
  :<|> "MakeMove" :>  Header "Uuid" UUID :> Header "CoordX" Int :> Header "CoordY" Int :> Get '[JSON] GameState

type SwaggerApi = SwaggerSchemaUI "swagger-ui" "swagger.json"

type ApiWithSwagger = Api :<|> SwaggerApi

apiProxy :: Proxy ApiWithSwagger
apiProxy = Proxy