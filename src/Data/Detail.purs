module Data.Detail (Detail(..)) where

import Data.Foreign.Class (class IsForeign, readProp)
import Data.Show (class Show, show)
import Prelude (($), (<>), bind, pure)

data Detail = Detail { id :: Int
                     , name :: String
                     , value :: String
                     }

instance detailShow :: Show Detail where
  show (Detail { id
               , name
               , value
               }) =
    "{"
    <> " \"id\": " <> show id <> ","
    <> " \"name\": " <> show name <> ","
    <> " \"value\": " <> show value
    <> " }"

instance detailIsForeign :: IsForeign Detail where
  read v = do
    id <- readProp "id" v
    name <- readProp "name" v
    value <- readProp "value" v
    pure $ Detail { id
                  , name
                  , value
                  }
