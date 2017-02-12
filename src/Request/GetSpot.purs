module Request.GetSpot (getSpot) where

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Foreign (F)
import Data.Foreign.Class (readJSON)
import Data.GetSpotResponse (GetSpotResponse)
import Data.Options ((:=))
import Fetch (HTTP, fetch)
import Fetch.Options (Method(..), headers, method, url) as FetchOptions
import Prelude (($), (<>), (<<<), bind, show)
import Request.Helper.Headers (headers)

getSpot :: forall eff
               . Int
               -> String
               -> Aff ( http :: HTTP
                      | eff
                      ) GetSpotResponse
getSpot spotId token = do
  text <- fetch options
  let f = readJSON text :: F GetSpotResponse
  makeAff (\ng ok -> either (ng <<< error <<< show) ok $ runExcept f)
  where
    url = ( "https://api.rallyapp.jp/spots/"
          <> show spotId
          <> "?view_type=admin"
          )
    options = FetchOptions.method := FetchOptions.GET
              <> FetchOptions.url := url
              <> FetchOptions.headers := (headers token)
