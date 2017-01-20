module Main where

import Prelude (Unit, ($), (==), (<>), bind, void)
import Command (args, command, name)
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Options ((:=))
import Fetch (HTTP, fetch)
import Fetch.Options (Method(..), method, url) as FetchOptions
import Node.Process (PROCESS, argv, exit) as Process

type Effs = (console :: CONSOLE, err :: EXCEPTION, http :: HTTP, process :: Process.PROCESS)

export :: Array String -> Eff Effs Unit
export _ = void $ launchAff do
  let url = "https://api.rallyapp.jp/rallies/kqahbgshyjwrbzwh"
  text <- fetch $ FetchOptions.method := FetchOptions.GET
                  <> FetchOptions.url := url
  liftEff $ log text
  liftEff $ Process.exit 0

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
