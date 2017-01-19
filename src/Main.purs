module Main where

import Prelude (Unit, ($), bind, show)
import Command (command)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Node.Process (PROCESS, argv, exit) as Process

type Effs = (console :: CONSOLE, process :: Process.PROCESS)

help :: Array String -> Eff Effs Unit
help _ = do
  log "rally <subcommand> [<args>]"
  log ""
  log "  help ... Show help"
  Process.exit 0

main :: Eff Effs Unit
main = do
  argv <- Process.argv
  log $ show $ command argv
  help []
