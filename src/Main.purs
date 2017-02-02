module Main where

import Command (args, command, name)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Command.Export (export)
import Command.Help (help)
import Fetch (HTTP)
import Node.Process (PROCESS, argv) as Process
import Prelude (Unit, ($), (==), bind)

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
