module Request.GetSpot (getSpot) where

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Foreign (F)
import Data.Foreign.Class (readJSON)
import Data.GetSpotResponse (GetSpotResponse)
import Data.Options ((:=))
import Data.StrMap (fromFoldable) as StrMap
import Data.Tuple (Tuple(..))
import Fetch (HTTP, fetch)
import Fetch.Options (Method(..), headers, method, url) as FetchOptions
import Prelude (($), (<>), (<<<), bind, show)

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
    headers = StrMap.fromFoldable [ Tuple "Content-Type" "application/json"
                                  , Tuple "User-Agent" "rally-cli"
                                  , Tuple "Authorization" $ "Token token=\"" <> token <> "\""
                                  ]
    url = ( "https://api.rallyapp.jp/spots/"
          <> show spotId
          <> "?view_type=admin"
          )
    options = FetchOptions.method := FetchOptions.GET
              <> FetchOptions.url := url
              <> FetchOptions.headers := headers
