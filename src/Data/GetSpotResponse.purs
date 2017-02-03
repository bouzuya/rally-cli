module Data.GetSpotResponse (GetSpotResponse(..)) where

import Data.Detail (Detail)
import Data.Foreign (F, Foreign, readString)
import Data.Foreign.Class (class IsForeign, readProp)
import Data.Foreign.Index (prop)
import Data.Foreign.Null (readNull, unNull)
import Data.Maybe (fromMaybe)
import Data.Show (class Show, show)
import Prelude (($), (<>), bind, pure)

data GetSpotResponse = GetSpotResponse
  { id :: Int
  , description :: String -- or null
  , details :: Array Detail
  -- , images :: Array Image
  , lat :: String -- NumberString
  , lng :: String -- NumberString
  , name :: String
  , radius :: String -- NumberString
  , stampByLocation :: Boolean
  , stampByQrCode :: Boolean
  , tagline :: String -- or null
  , zoom :: Int
  }

instance getSpotResponseShow :: Show GetSpotResponse where
  show (GetSpotResponse { id
                        , description
                        , details
                        -- , images :: Array Image
                        , lat
                        , lng
                        , name
                        , radius
                        , stampByLocation
                        , stampByQrCode
                        , tagline
                        , zoom
                        }) =
    "{"
    <> " \"id\": " <> show id <> ","
    <> " \"description\": " <> show description <> ","
    <> " \"details\": " <> show details <> ","
    <> " \"lat\": " <> show lat <> ","
    <> " \"lng\": " <> show lng <> ","
    <> " \"name\": " <> show name <> ","
    <> " \"radius\": " <> show radius <> ","
    <> " \"stampByLocation\": " <> show stampByLocation <> ","
    <> " \"stampByQrCode\": " <> show stampByQrCode <> ","
    <> " \"tagline\": " <> show tagline <> ","
    <> " }"

readNullProp :: String -> String -> Foreign -> F String
readNullProp p defaultValue o = do
  pv <- prop p o
  ns <- readNull readString pv
  pure $ fromMaybe defaultValue $ unNull ns

instance getSpotResponseIsForeign :: IsForeign GetSpotResponse where
  read value = do
    id <- readProp "id" value
    description <- readNullProp "description" "" value
    details <- readProp "details" value
    lat <- readProp "lat" value
    lng <- readProp "lng" value
    name <- readProp "name" value
    radius <- readProp "radius" value
    stampByLocation <- readProp "stampByLocation" value
    stampByQrCode <- readProp "stampByQrCode" value
    tagline <- readNullProp "tagline" "" value
    zoom <- readProp "zoom" value
    pure $ GetSpotResponse { id
                           , description
                           , details
                           -- , images :: Array Image
                           , lat
                           , lng
                           , name
                           , radius
                           , stampByLocation
                           , stampByQrCode
                           , tagline
                           , zoom
                           }
