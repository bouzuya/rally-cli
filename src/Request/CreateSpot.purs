module Request.CreateSpot (createSpot) where

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (runExcept)
import Data.CreateSpotResponse (CreateSpotResponse)
import Data.Either (either)
import Data.Foreign (F)
import Data.Foreign.Class (readJSON)
import Data.Options ((:=))
import Fetch (HTTP, fetch)
import Fetch.Options (Method(..), body, headers, method, url) as FetchOptions
import Prelude (($), (<>), (<<<), bind, show)
import Request.Helper.P (p, ps)
import Request.Helper.Headers (headers)

body :: String -> String
body displayName =
  ps [ p "view_type" "admin"
     , p "name" displayName
     ]

url :: String -> String
url stampRallyId =
  "https://api.rallyapp.jp/stamp_rallies/" <> stampRallyId <> "/spots"

createSpot
  :: forall eff
   . String
  -> String
  -> String
  -> Aff ( http :: HTTP
         | eff
         ) CreateSpotResponse
createSpot stampRallyId displayName token = do
  text <- fetch options
  let f = readJSON text :: F CreateSpotResponse
  makeAff (\ng ok -> either (ng <<< error <<< show) ok $ runExcept f)
  where
    options = FetchOptions.method := FetchOptions.POST
              <> FetchOptions.url := (url stampRallyId)
              <> FetchOptions.headers := (headers token)
              <> FetchOptions.body := (body displayName)
