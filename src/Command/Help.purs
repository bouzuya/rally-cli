module Command.Help (help) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Unit (Unit)
import Node.Process (PROCESS, exit)
import Prelude (bind)

help :: forall eff
        . Array String
        -> Eff ( console :: CONSOLE
               , process :: PROCESS
               | eff
               ) Unit
help _ = do
  log "Usage: rally <command> [<args>]"
  log ""
  log "Commands:"
  log "  export"
  log "  help"
  exit 0
