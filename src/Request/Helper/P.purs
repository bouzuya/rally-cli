module Request.Helper.P
  ( P
  , p
  , ps
  ) where

import Data.Show (class Show)
import Data.Traversable (class Traversable, intercalate)
import Prelude ((<>), (<$>), ($), show)

data P = P String String

instance showP :: Show P where
  show (P k v) = show k <> ": " <> v

p :: forall a. (Show a) => String -> a -> P
p k v = P k $ show v

ps :: forall t. (Traversable t) => t P -> String
ps a = "{ " <> (intercalate ", " $ show <$> a) <> " }"
