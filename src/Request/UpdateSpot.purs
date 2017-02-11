module Request.UpdateSpot (updateSpot) where

import Control.Monad.Aff (Aff)
import Data.GetSpotResponse (GetSpotResponse(..))
import Data.Options ((:=))
import Data.StrMap (fromFoldable) as StrMap
import Data.Tuple (Tuple(..))
import Fetch (HTTP, fetch)
import Fetch.Options (Method(..), body, headers, method, url) as FetchOptions
import Prelude (Unit, ($), (<>), (<$>), const, show, unit)

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
  "{"
  <> " " <> show "view_type" <> ":" <> show "admin" <> ","
  <> " " <> show "description" <> ":" <> show description <> ","
  -- TODO: details
  <> " " <> show "lat" <> ":" <> show lat <> ","
  <> " " <> show "lng" <> ":" <> show lng <> ","
  <> " " <> show "name" <> ":" <> show name <> ","
  <> " " <> show "radius" <> ":" <> show radius <> ","
  <> " " <> show "stamp_by_location" <> ":" <> show stampByLocation <> ","
  <> " " <> show "stamp_by_qr_code" <> ":" <> show stampByQrCode <> ","
  <> " " <> show "tagline" <> ":" <> show tagline <> ","
  <> " " <> show "zoom" <> ":" <> show zoom
  <> " }"

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
    headers = StrMap.fromFoldable [ Tuple "Content-Type" "application/json"
                                  , Tuple "User-Agent" "rally-cli"
                                  , Tuple "Authorization" $ "Token token=\"" <> token <> "\""
                                  ]
    options = FetchOptions.method := FetchOptions.PATCH
              <> FetchOptions.url := (url spotId)
              <> FetchOptions.headers := headers
              <> FetchOptions.body := (body spot)
