module Data.GetSpotsResponse (GetSpotsItem(..), GetSpotsResponse(..)) where

import Data.Foreign.Class (class IsForeign, read, readProp)
import Data.Foreign.Index (prop)
import Data.Show (class Show, show)
import Prelude (($), (<>), bind, map, pure)

data GetSpotsItem = GetSpotsItem { id :: Int }
data GetSpotsResponse = GetSpotsResponse { ids :: Array GetSpotsItem }

instance getSpotsItemShow :: Show GetSpotsItem where
  show (GetSpotsItem { id }) = "{ \"id\": " <> show id <> " }"

instance getSpotsItemIsForeign :: IsForeign GetSpotsItem where
  read value = do
    id <- readProp "id" value
    pure $ GetSpotsItem { id }

instance getSpotsResponseShow :: Show GetSpotsResponse where
  show (GetSpotsResponse { ids }) = show (map show ids)

instance getSpotsResponseIsForeign :: IsForeign GetSpotsResponse where
  read value = do
    spotsForeign <- prop "spots" value
    ids <- read spotsForeign
    pure $ GetSpotsResponse { ids }
