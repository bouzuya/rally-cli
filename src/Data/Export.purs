module Data.Export (Export(..)) where

import Data.Foreign.Class (class IsForeign, readProp)
import Data.GetSpotResponse (GetSpotResponse)
import Data.GetStampRallyResponse (GetStampRallyResponse)
import Data.Show (class Show, show)
import Prelude (($), (<>), bind, pure)

data Export = Export { spots :: Array GetSpotResponse
                     , stampRally :: GetStampRallyResponse
                     }

instance exportShow :: Show Export where
  show (Export { spots
               , stampRally
               }) =
    "{"
    <> " \"spots\": " <> show spots <> ","
    <> " \"stampRally\": " <> show stampRally
    <> " }"

instance exportIsForeign :: IsForeign Export where
  read v = do
    spots <- readProp "spots" v
    stampRally <- readProp "stampRally" v
    pure $ Export { spots
                  , stampRally
                  }
