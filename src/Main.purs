module Main where

import Prelude (Unit, ($), bind, show)
import Command (command)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Node.Process (PROCESS, argv) as Process

main :: forall e. Eff (console :: CONSOLE, process :: Process.PROCESS | e) Unit
main = do
  argv <- Process.argv
  log $ show $ command argv
