module Data.CreateStampRallyResponse (CreateStampRallyResponse(..)) where

import Data.Foreign.Class (class IsForeign, readProp)
import Data.Show (class Show, show)
import Prelude (($), (<>), bind, pure)

data CreateStampRallyResponse = CreateStampRallyResponse { id :: String }

instance createStampRallyResponseShow :: Show CreateStampRallyResponse where
  show (CreateStampRallyResponse { id }) = "{ \"id\": " <> show id <> " }"

instance createStampRallyResponseIsForeign
  :: IsForeign CreateStampRallyResponse where
  read value = do
    id <- readProp "id" value
    pure $ CreateStampRallyResponse { id }
