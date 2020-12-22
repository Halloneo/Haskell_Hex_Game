{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Server (serverApplication) where

import Prelude hiding (lookup, Left, Right)
import Servant
import Servant.Swagger
import Servant.Swagger.UI
import Data.Swagger
import Data.UUID (UUID)
import Control.Lens
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Concurrent.STM.Map (Map, insert, lookup)
import Control.Monad.Trans.Class (lift)
import Control.Concurrent.STM (atomically)


import Types
import Api

type Cache = Map UUID GameState
type AppM = ReaderT Cache Handler

gameServer :: ServerT ApiWithSwagger AppM
gameServer =
    (startGame
    :<|> getGameState
    :<|> makeMove)
    :<|> swaggerDocServer

startGame :: AppM GameState
startGame = do
    gameStorage <- ask
    newGameState <- liftIO initialGameState
    let newGameGuid = uuid $ newGameState
    liftIO $ atomically $ insert newGameGuid newGameState gameStorage
    return newGameState

getGameState :: Maybe UUID -> AppM GameState
getGameState Nothing = failingHandler "Uuid header is required!"
getGameState (Just gameUuid) = do
    gameStorage <- ask
    gameStateMaybe <- liftIO $ atomically $ lookup gameUuid gameStorage
    case gameStateMaybe of
        Just gameState -> return gameState
        Nothing -> failingHandler $ "Game with UUID '" ++ show gameUuid ++ "' does not exist!"

makeMove :: Maybe UUID -> Maybe Int -> Maybe Int -> AppM GameState
makeMove Nothing Nothing Nothing = failingHandler "Parameters not provided"
makeMove Nothing _ _ = failingHandler "UUID not provided"
makeMove _ Nothing _ = failingHandler "CoordX not provided"
makeMove _ _ Nothing = failingHandler "CoordY not provided"
makeMove (Just guid) (Just coordX) (Just coordY) = do
  gameStorage <- ask
  gameStateMaybe <- liftIO $ atomically $ lookup guid gameStorage
  case gameStateMaybe of
      Nothing -> failingHandler $ "Game with UUID '" ++ show guid ++ "' does not exist!"
      Just gameState -> do
        let newGameState = makeMoveInternal gameState (coordX, coordY)
        liftIO $ atomically $ insert guid newGameState gameStorage
        return newGameState

failingHandler message = throwError $ err400 { errReasonPhrase = message }

serverApplication :: Cache -> Application
serverApplication gameStorage =
    serve apiProxy $
    hoistServer apiProxy ((flip runReaderT) gameStorage) gameServer

swagger :: Swagger
swagger = toSwagger (Proxy :: Proxy Api)
    & info.title       .~ "2048 API"
    & info.version     .~ "1.0"
    & info.description ?~ "API for communicating with game server."

swaggerFromHandlerToAppM :: Server SwaggerApi -> ServerT SwaggerApi AppM
swaggerFromHandlerToAppM = hoistServer (Proxy :: Proxy SwaggerApi) lift

swaggerDocServer :: ServerT SwaggerApi AppM
swaggerDocServer = swaggerFromHandlerToAppM $ swaggerSchemaUIServer swagger