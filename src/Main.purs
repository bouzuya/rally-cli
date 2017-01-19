module Main where

import Prelude (Unit, ($), (==), bind)
import Command (args, command, name)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Node.Process (PROCESS, argv, exit) as Process

type Effs = (console :: CONSOLE, process :: Process.PROCESS)

export :: Array String -> Eff Effs Unit
export _ = do
  log "(export)"
  Process.exit 0

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
