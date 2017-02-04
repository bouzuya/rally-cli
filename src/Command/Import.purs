module Command.Import (import_) where

import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.CreateTokenResponse (CreateTokenResponse(..))
import Data.Maybe (fromMaybe)
import Data.StrMap (lookup) as StrMap
import Fetch (HTTP)
import Node.Process (PROCESS, getEnv, exit) as Process
import Prelude (Unit, ($), (<>), bind, pure, void)
import Request.CreateToken (createToken)

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

import_ :: forall eff
          . Array String
          -> Eff ( console :: CONSOLE
                 , err :: EXCEPTION
                 , http :: HTTP
                 , process :: Process.PROCESS
                 | eff
                 ) Unit
import_ _ = void $ launchAff do
  { email, password, stampRallyId } <- liftEff $ params
  (CreateTokenResponse { token }) <- createToken email password
  liftEff $ log $ "import: " <> token
  liftEff $ Process.exit 0
