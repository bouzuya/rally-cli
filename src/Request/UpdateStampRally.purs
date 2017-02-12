module Request.UpdateStampRally (updateStampRally) where

import Control.Monad.Aff (Aff)
import Data.GetStampRallyResponse (GetStampRallyResponse(..))
import Data.Options ((:=))
import Fetch (HTTP, fetch)
import Fetch.Options (Method(..), body, headers, method, url) as FetchOptions
import Prelude (Unit, (<>), (<$>), const, unit)
import Request.Helper.P (p, ps)
import Request.Helper.Headers (headers)

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
  ps [ p "time_zone" "+09:00"
     , p "view_type" "admin"
     , p "description" description
     -- TODO: details
     , p "display" display
     , p "display_end_datetime" displayEndDatetime
     , p "display_name" displayName
     , p "display_start_datetime" displayStartDatetime
     , p "end_datetime" endDatetime
     -- TODO: images
     , p "map_visible" mapVisible
     , p "open" open
     , p "spot_radius_default" spotRadiusDefault
     , p "spot_stamp_by_location_default" spotStampByLocationDefault
     , p "spot_stamp_by_qr_code_default" spotStampByQrCodeDefault
     , p "start_datetime" startDatetime
     , p "tagline" tagline
     , p "theme_background_pattern" themeBackgroundPattern
     , p "theme_reward_pattern" themeRewardPattern
     , p "theme_spot_pattern" themeSpotPattern
     ]

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
    options = FetchOptions.method := FetchOptions.PATCH
              <> FetchOptions.url := (url stampRallyId)
              <> FetchOptions.headers := (headers token)
              <> FetchOptions.body := (body stampRally)
