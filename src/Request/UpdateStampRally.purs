module Request.UpdateStampRally (updateStampRally) where

import Control.Monad.Aff (Aff)
import Data.GetStampRallyResponse (GetStampRallyResponse(..))
import Data.Options ((:=))
import Data.StrMap (fromFoldable) as StrMap
import Data.Tuple (Tuple(..))
import Fetch (HTTP, fetch)
import Fetch.Options (Method(..), body, headers, method, url) as FetchOptions
import Prelude (Unit, ($), (<>), (<$>), const, show, unit)

body :: GetStampRallyResponse -> String
body (GetStampRallyResponse { description
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
  "{"
  <> " " <> show "time_zone" <> ":" <> show "+09:00" <> ","
  <> " " <> show "view_type" <> ":" <> show "admin" <> ","
  <> " " <> show "description" <> ":" <> show description <> ","
  -- TODO: details
  <> " " <> show "display" <> ":" <> show display <> ","
  <> " " <> show "display_end_datetime" <> ":" <> show displayEndDatetime <> ","
  <> " " <> show "display_name" <> ":" <> show displayName <> ","
  <> " " <> show "display_start_datetime" <> ":" <> show displayStartDatetime <> ","
  <> " " <> show "end_datetime" <> ":" <> show endDatetime <> ","
  -- TODO: images
  <> " " <> show "map_visible" <> ":" <> show mapVisible <> ","
  <> " " <> show "open" <> ":" <> show open <> ","
  <> " " <> show "spot_radius_default" <> ":" <> show spotRadiusDefault <> ","
  <> " " <> show "spot_stamp_by_location_default" <> ":" <> show spotStampByLocationDefault <> ","
  <> " " <> show "spot_stamp_by_qr_code_default" <> ":" <> show spotStampByQrCodeDefault <> ","
  <> " " <> show "start_datetime" <> ":" <> show startDatetime <> ","
  <> " " <> show "tagline" <> ":" <> show tagline <> ","
  <> " " <> show "theme_background_pattern" <> ":" <> show themeBackgroundPattern <> ","
  <> " " <> show "theme_reward_pattern" <> ":" <> show themeRewardPattern <> ","
  <> " " <> show "theme_spot_pattern" <> ":" <> show themeSpotPattern
  <> " }"

url :: String -> String
url stampRallyId = "https://api.rallyapp.jp/stamp_rallies/" <> stampRallyId

updateStampRally
  :: forall eff
   . String
  -> GetStampRallyResponse
  -> String
  -> Aff ( http :: HTTP
         | eff
         ) Unit
updateStampRally stampRallyId stampRally token = const unit <$> fetch options
  where
    headers = StrMap.fromFoldable [ Tuple "Content-Type" "application/json"
                                  , Tuple "User-Agent" "rally-cli"
                                  , Tuple "Authorization" $ "Token token=\"" <> token <> "\""
                                  ]
    options = FetchOptions.method := FetchOptions.PATCH
              <> FetchOptions.url := (url stampRallyId)
              <> FetchOptions.headers := headers
              <> FetchOptions.body := (body stampRally)
