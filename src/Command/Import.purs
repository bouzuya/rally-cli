module Command.Import (import_) where

import Control.Monad.Aff (Aff, launchAff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, error, try)
import Control.Monad.Except (runExcept)
import Data.CreateStampRallyResponse (CreateStampRallyResponse(..))
import Data.CreateTokenResponse (CreateTokenResponse(..))
import Data.Either (Either(Left, Right), either)
import Data.Export (Export(..))
import Data.GetStampRallyResponse (GetStampRallyResponse(..))
import Data.Foreign.Class (readJSON)
import Data.Maybe (fromMaybe)
import Data.StrMap (lookup) as StrMap
import Fetch (HTTP)
import Node.Process (PROCESS, exit, getEnv, stdin) as Process
import Prelude (Unit, ($), (<>), (<<<), bind, pure, show, unit, void)
import Request.CreateStampRally (createStampRally)
import Request.CreateToken (createToken)
import Request.UpdateStampRally (updateStampRally)
import Stdin (read) as Stdin

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

readExport
  :: forall e
   . String
  -> Aff e Export
readExport s =
  makeAff \ng ok -> either (ng <<< error <<< show) ok $ runExcept $ readJSON s

createStampRally'
  :: forall e
   . GetStampRallyResponse
  -> String
  -> Aff ( http :: HTTP | e ) String
createStampRally' stampRally@(GetStampRallyResponse { displayName }) token = do
  (CreateStampRallyResponse { id: newId }) <- createStampRally displayName token
  updateStampRally newId stampRally token
  pure newId

launchAff'
  :: forall e
   . Eff ( console :: CONSOLE
         , err :: EXCEPTION
         , http :: HTTP
         , process :: Process.PROCESS
         | e
         ) Unit
launchAff' =
  void $ launchAff do
    { email, password, stampRallyId } <- liftEff $ params
    s <- Stdin.read Process.stdin
    (Export { stampRally }) <- readExport s
    (CreateTokenResponse { token }) <- createToken email password
    newId <- createStampRally' stampRally token
    liftEff $ log $ "https://admin.rallyapp.jp/#/rallies/" <> newId
    liftEff $ Process.exit 0

catchException
  :: forall e
   . Eff ( console :: CONSOLE
         , err :: EXCEPTION
         , process :: Process.PROCESS
         | e
         ) Unit
  -> Eff ( console :: CONSOLE
         , process :: Process.PROCESS
         | e
         ) Unit
catchException eff = do
  e <- try eff
  case e of
    (Left l) -> do
      log $ show l
      Process.exit 1
    (Right _) -> pure unit

import_ :: forall eff
          . Array String
          -> Eff ( console :: CONSOLE
                 , http :: HTTP
                 , process :: Process.PROCESS
                 | eff
                 ) Unit
import_ _ = catchException launchAff'
