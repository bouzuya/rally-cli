module Main where

import Command (args, command, name)
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import CreateToken (createToken)
import GetStampRally (getStampRally)
import Data.CreateTokenResponse (CreateTokenResponse(..))
import Data.Maybe (maybe)
import Data.StrMap (lookup) as StrMap
import Fetch (HTTP)
import Node.Process (PROCESS, argv, getEnv, exit) as Process
import Prelude (Unit, ($), (==), bind, id, show, void)

type Effs = (console :: CONSOLE, err :: EXCEPTION, http :: HTTP, process :: Process.PROCESS)

-- process.env.EMAIL='<email>'
-- process.env.PASSWORD='<password>'
export :: Array String -> Eff Effs Unit
export _ = void $ launchAff do
  env <- liftEff $ Process.getEnv
  let email = maybe "" id $ StrMap.lookup "EMAIL" env
  let password = maybe "" id $ StrMap.lookup "PASSWORD" env
  token <- createToken email password
  liftEff $ log $ show token
  do
    let stampRallyId = maybe "" id $ StrMap.lookup "STAMP_RALLY_ID" env
    liftEff $ log stampRallyId
    stampRally <- getStampRally stampRallyId $ accessToken token
    liftEff $ log $ show stampRally
  liftEff $ Process.exit 0
  where
    accessToken (CreateTokenResponse { token }) = token

help :: Array String -> Eff Effs Unit
help _ = do
  log "Usage: rally <command> [<args>]"
  log ""
  log "Commands:"
  log "  export"
  log "  help"
  Process.exit 0

main :: Eff Effs Unit
main = do
  argv <- Process.argv
  let c = command argv
  if name c == "export" then export $ args c else help []
