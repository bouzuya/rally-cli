module Main where

import Prelude (class Show, Unit, ($), (<>), bind, id, show)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (drop, head, tail) as Array
import Data.Maybe (maybe)
import Node.Process (PROCESS, argv) as Process

data Command = Command String (Array String)

instance commandShow :: Show Command where
  show (Command name args) = name <> " " <> show args

command :: Array String -> Command
command argv = Command name commandArgs
  where
    args = Array.drop 2 argv
    name = maybe "help" id $ Array.head args
    commandArgs = maybe [] id $ Array.tail args

main :: forall e. Eff (console :: CONSOLE, process :: Process.PROCESS | e) Unit
main = do
  argv <- Process.argv
  log $ show $ command argv
