module Data.GetStampRallyResponse where

import Data.Foreign (F, Foreign, readString)
import Data.Foreign.Class (class IsForeign, readProp)
import Data.Foreign.Index (prop)
import Data.Foreign.Null (readNull, unNull)
import Data.Maybe (maybe)
import Data.Show (class Show, show)
import Prelude (($), (<>), bind, id, pure)

data Detail = Detail { id :: Int
                     , name :: String
                     , value :: String
                     }

instance detailShow :: Show Detail where
  show (Detail { id
               , name
               , value
               }) =
    "(Detail { "
    <> "id: " <> show id <> ", "
    <> "name: " <> show name <> ", "
    <> "value: " <> show value <> " "
    <> "})"

instance detailIsForeign :: IsForeign Detail where
  read v = do
    id <- readProp "id" v
    name <- readProp "name" v
    value <- readProp "value" v
    pure $ Detail { id
                  , name
                  , value
                  }

data GetStampRallyResponse = GetStampRallyResponse
  { id :: String
  , description :: String
  , details :: Array Detail
  , display :: Boolean
  , displayEndDatetime :: String -- DateTimeString
  , displayName :: String
  , displayStartDatetime :: String -- DateTimeString
  , endDatetime :: String -- DateTimeString
  -- , images :: [] -- TODO
  , mapVisible :: Boolean
  , open :: Boolean
  , spotRadiusDefault :: Number
  , spotStampByLocationDefault :: Boolean
  , spotStampByQrCodeDefault :: Boolean
  , startDatetime :: String -- DateTimeString
  , tagline :: String
  , themeBackgroundPattern :: Int
  , themeRewardPattern :: Int
  , themeSpotPattern :: Int
  }

instance getStampRallyResponseShow :: Show GetStampRallyResponse where
  show (GetStampRallyResponse { id
                              , details
                              , description
                              , display
                              , displayEndDatetime
                              , displayName
                              , displayStartDatetime
                              , endDatetime
                              , mapVisible
                              , open
                              , spotRadiusDefault
                              , spotStampByLocationDefault
                              , spotStampByQrCodeDefault
                              , startDatetime
                              , tagline
                              , themeBackgroundPattern
                              , themeRewardPattern
                              , themeSpotPattern
                              }) =
    "(GetStampRallyResponse { "
    <> "id: " <> show id <> ", "
    <> "details: " <> show details <> ", "
    <> "description: " <> show description <> ", "
    <> "display: " <> show display <> ", "
    <> "displayEndDatetime: " <> show displayEndDatetime <> ", "
    <> "displayName: " <> show displayName <> ", "
    <> "displayStartDatetime: " <> show displayStartDatetime <> ", "
    <> "endDatetime: " <> show endDatetime <> ", "
    <> "mapVisible: " <> show mapVisible <> ", "
    <> "open: " <> show open <> ", "
    <> "spotRadiusDefault: " <> show spotRadiusDefault <> ", "
    <> "spotStampByLocationDefault: "
      <> show spotStampByLocationDefault
      <> ", "
    <> "spotStampByQrCodeDefault: " <> show spotStampByQrCodeDefault <> ", "
    <> "startDatetime: " <> show startDatetime <> ", "
    <> "tagline: " <> show tagline <> ", "
    <> "themeBackgroundPattern: " <> show themeBackgroundPattern <> ", "
    <> "themeRewardPattern: " <> show themeRewardPattern <> ", "
    <> "themeSpotPattern: " <> show themeSpotPattern <> " "
    <> "})"

readNullProp :: String -> String -> Foreign -> F String
readNullProp p defaultValue o = do
  pv <- prop p o
  ns <- readNull readString pv
  pure $ maybe defaultValue id $ unNull ns

instance getStampRallyResponseIsForeign :: IsForeign GetStampRallyResponse where
  read value = do
    id <- readProp "id" value
    details <- readProp "details" value
    description <- readNullProp "description" "" value
    display <- readProp "display" value
    displayEndDatetime <- readProp "displayEndDatetime" value
    displayName <- readProp "displayName" value
    displayStartDatetime <- readProp "displayStartDatetime" value
    endDatetime <- readProp "endDatetime" value
    mapVisible <- readProp "mapVisible" value
    open <- readProp "open" value
    spotRadiusDefault <- readProp "spotRadiusDefault" value
    spotStampByLocationDefault <- readProp "spotStampByLocationDefault" value
    spotStampByQrCodeDefault <- readProp "spotStampByQrCodeDefault" value
    startDatetime <- readProp "startDatetime" value
    tagline <- readNullProp "tagline" "" value
    themeBackgroundPattern <- readProp "themeBackgroundPattern" value
    themeRewardPattern <- readProp "themeRewardPattern" value
    themeSpotPattern <- readProp "themeSpotPattern" value
    pure $ GetStampRallyResponse { id
                                 , details
                                 , description
                                 , display
                                 , displayEndDatetime
                                 , displayName
                                 , displayStartDatetime
                                 , endDatetime
                                 , mapVisible
                                 , open
                                 , spotRadiusDefault
                                 , spotStampByLocationDefault
                                 , spotStampByQrCodeDefault
                                 , startDatetime
                                 , tagline
                                 , themeBackgroundPattern
                                 , themeRewardPattern
                                 , themeSpotPattern
                                 }
