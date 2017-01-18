module Main where

import Prelude (Unit, ($), (<>), bind, show)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (drop) as Array
import Node.Process (PROCESS, argv) as Process

main :: forall e. Eff (console :: CONSOLE, process :: Process.PROCESS | e) Unit
main = do
  argv <- Process.argv
  let args = Array.drop 2 $ argv
  log $ "args: " <> show args
