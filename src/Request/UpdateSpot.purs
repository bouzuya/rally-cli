module Request.UpdateSpot (updateSpot) where

import Control.Monad.Aff (Aff)
import Data.GetSpotResponse (GetSpotResponse(..))
import Data.Options ((:=))
import Fetch (HTTP, fetch)
import Fetch.Options (Method(..), body, headers, method, url) as FetchOptions
import Prelude (Unit, (<>), (<$>), const, show, unit)
import Request.Helper.P (p, ps)
import Request.Helper.Headers (headers)

body :: GetSpotResponse -> String
body (GetSpotResponse { description
                      -- , details :: Array Detail
                      , lat
                      , lng
                      , name
                      , radius
                      , stampByLocation
                      , stampByQrCode
                      , tagline
                      , zoom
                      }) =
  ps [ p "view_type" "admin"
     , p "description" description
     -- TODO: details
     , p "lat" lat
     , p "lng" lng
     , p "name" name
     , p "radius" radius
     , p "stamp_by_location" stampByLocation
     , p "stamp_by_qr_code" stampByQrCode
     , p "tagline" tagline
     , p "zoom" zoom
     ]

url :: Int -> String
url spotId = "https://api.rallyapp.jp/spots/" <> show spotId

updateSpot
  :: forall eff
   . Int
  -> GetSpotResponse
  -> String
  -> Aff ( http :: HTTP
         | eff
         ) Unit
updateSpot spotId spot token = const unit <$> fetch options
  where
    options = FetchOptions.method := FetchOptions.PATCH
              <> FetchOptions.url := (url spotId)
              <> FetchOptions.headers := (headers token)
              <> FetchOptions.body := (body spot)
