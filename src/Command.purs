module Command (Command, command, name, args) where

import Prelude (class Show, ($), (<>), id, show)
import Data.Array (drop, head, tail) as Array
import Data.Maybe (maybe) as Maybe

data Command = Command String (Array String)

instance commandShow :: Show Command where
  show (Command n a) = n <> " " <> show a

command :: Array String -> Command
command argv = Command n a
  where
    n = Maybe.maybe "help" id $ Array.head $ Array.drop 2 argv
    a = Maybe.maybe [] id $ Array.tail $ Array.drop 2 argv

name :: Command -> String
name (Command n _) = n

args :: Command -> Array String
args (Command _ a) = a
