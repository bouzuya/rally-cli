module Command.Export (export) where

import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.CreateTokenResponse (CreateTokenResponse(..))
import Data.Functor (mapFlipped)
import Data.GetSpotResponse (GetSpotResponse)
import Data.GetSpotsResponse (GetSpotsResponse(..), GetSpotsItem(..))
import Data.GetStampRallyResponse (GetStampRallyResponse)
import Data.Maybe (fromMaybe)
import Data.StrMap (lookup) as StrMap
import Data.Traversable (sequence)
import Fetch (HTTP)
import Node.Process (PROCESS, getEnv, exit) as Process
import Prelude (Unit, ($), bind, pure, show, void)
import Request.CreateToken (createToken)
import Request.GetSpot (getSpot)
import Request.GetSpots (getSpots)
import Request.GetStampRally (getStampRally)

getSpots' :: forall eff
                  . CreateTokenResponse
                  -> String
                  -> Aff ( http :: HTTP
                         | eff
                         ) (Array GetSpotResponse)
getSpots' (CreateTokenResponse { token }) stampRallyId = do -- A a b
  (GetSpotsResponse { ids }) <- getSpots stampRallyId token
  sequence $ mapFlipped ids \(GetSpotsItem { id }) -> getSpot id token

getStampRally' :: forall eff
                  . CreateTokenResponse
                  -> String
                  -> Aff ( http :: HTTP
                         | eff
                         ) GetStampRallyResponse
getStampRally' (CreateTokenResponse { token }) stampRallyId = do
  getStampRally stampRallyId token

-- process.env.EMAIL='<email>'
-- process.env.PASSWORD='<password>'
-- process.env.STAMP_RALLY_ID='<stamp_rally_id>'
params :: forall eff
          . Eff ( process :: Process.PROCESS
                | eff
                ) { email :: String
                  , password :: String
                  , stampRallyId :: String
                  }
params = do
  env <- Process.getEnv
  let email = fromMaybe "" $ StrMap.lookup "EMAIL" env
  let password = fromMaybe "" $ StrMap.lookup "PASSWORD" env
  let stampRallyId = fromMaybe "" $ StrMap.lookup "STAMP_RALLY_ID" env
  pure { email, password, stampRallyId }

export :: forall eff
          . Array String
          -> Eff ( console :: CONSOLE
                 , err :: EXCEPTION
                 , http :: HTTP
                 , process :: Process.PROCESS
                 | eff
                 ) Unit
export _ = void $ launchAff do
  { email, password, stampRallyId } <- liftEff $ params
  token <- createToken email password
  liftEff $ log $ show token
  stampRally <- getStampRally' token stampRallyId
  spots <- getSpots' token stampRallyId
  liftEff $ log $ show stampRally
  liftEff $ log $ show spots
  liftEff $ Process.exit 0
