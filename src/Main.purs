module Main where

import Command (args, command, name)
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.CreateTokenResponse (CreateTokenResponse(..))
import Data.GetStampRallyResponse (GetStampRallyResponse)
import Data.Maybe (maybe)
import Data.StrMap (lookup) as StrMap
import Fetch (HTTP)
import Node.Process (PROCESS, argv, getEnv, exit) as Process
import Prelude (Unit, ($), (==), bind, id, pure, show, void)
import Request.CreateToken (createToken)
import Request.GetStampRally (getStampRally)

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
  let email = maybe "" id $ StrMap.lookup "EMAIL" env
  let password = maybe "" id $ StrMap.lookup "PASSWORD" env
  let stampRallyId = maybe "" id $ StrMap.lookup "STAMP_RALLY_ID" env
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
  liftEff $ log $ show stampRally
  liftEff $ Process.exit 0

help :: forall eff
        . Array String
        -> Eff ( console :: CONSOLE
               , process :: Process.PROCESS
               | eff
               ) Unit
help _ = do
  log "Usage: rally <command> [<args>]"
  log ""
  log "Commands:"
  log "  export"
  log "  help"
  Process.exit 0

main :: forall eff
        . Eff ( console :: CONSOLE
              , err :: EXCEPTION
              , http :: HTTP
              , process :: Process.PROCESS
              | eff
              ) Unit
main = do
  argv <- Process.argv
  let c = command argv
  if name c == "export" then export $ args c else help []
