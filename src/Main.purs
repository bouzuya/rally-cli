module Main where

import Command (args, command, name)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Command.Export (export)
import Fetch (HTTP)
import Node.Process (PROCESS, argv, exit) as Process
import Prelude (Unit, ($), (==), bind)

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
