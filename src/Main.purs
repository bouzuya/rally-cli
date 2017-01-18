module Main where

import Prelude (Unit, ($), (<>), bind, id, show)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (drop, head) as Array
import Data.Maybe (maybe)
import Node.Process (PROCESS, argv) as Process

main :: forall e. Eff (console :: CONSOLE, process :: Process.PROCESS | e) Unit
main = do
  argv <- Process.argv
  let args = Array.drop 2 $ argv
  let subcommand = maybe "help" id $ Array.head args
  log $ "args: " <> show args
  log $ "subcommand: " <> subcommand
