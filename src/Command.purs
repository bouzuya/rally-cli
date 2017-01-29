module Command (Command, command, name, args) where

import Data.Array (drop, head, tail) as Array
import Data.Maybe (fromMaybe) as Maybe
import Prelude (class Show, ($), (<>), show)

data Command = Command String (Array String)

instance commandShow :: Show Command where
  show (Command n a) = n <> " " <> show a

command :: Array String -> Command
command argv = Command n a
  where
    n = Maybe.fromMaybe "help" $ Array.head $ Array.drop 2 argv
    a = Maybe.fromMaybe [] $ Array.tail $ Array.drop 2 argv

name :: Command -> String
name (Command n _) = n

args :: Command -> Array String
args (Command _ a) = a
