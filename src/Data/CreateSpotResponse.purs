module Data.CreateSpotResponse (CreateSpotResponse(..)) where

import Data.Foreign.Class (class IsForeign, readProp)
import Data.Show (class Show, show)
import Prelude (($), (<>), bind, pure)

data CreateSpotResponse = CreateSpotResponse { id :: Int }

instance createSpotResponseShow :: Show CreateSpotResponse where
  show (CreateSpotResponse { id }) = "{ \"id\": " <> show id <> " }"

instance createSpotResponseIsForeign
  :: IsForeign CreateSpotResponse where
  read value = do
    id <- readProp "id" value
    pure $ CreateSpotResponse { id }
