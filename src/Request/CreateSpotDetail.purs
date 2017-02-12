module Request.CreateSpotDetail (createSpotDetail) where

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (runExcept)
import Data.Detail (Detail(..))
import Data.Either (either)
import Data.Foreign (F)
import Data.Foreign.Class (readJSON)
import Data.Options ((:=))
import Data.StrMap (fromFoldable) as StrMap
import Data.Tuple (Tuple(..))
import Fetch (HTTP, fetch)
import Fetch.Options (Method(..), body, headers, method, url) as FetchOptions
import Prelude (($), (<>), (<<<), bind, show)
import Request.Helper.P (p, ps)

body :: Detail -> String
body (Detail { name, value }) =
  ps [ p "view_type" "admin"
     , p "name" name
     , p "value" value
     ]

url :: Int -> String
url spotId =
  "https://api.rallyapp.jp/spots/" <> show spotId <> "/details"

createSpotDetail
  :: forall eff
   . Int
  -> Detail
  -> String
  -> Aff ( http :: HTTP
         | eff
         ) Detail
createSpotDetail spotId detail token = do
  text <- fetch options
  let f = readJSON text :: F Detail
  makeAff (\ng ok -> either (ng <<< error <<< show) ok $ runExcept f)
  where
    headers = StrMap.fromFoldable [ Tuple "Content-Type" "application/json"
                                  , Tuple "User-Agent" "rally-cli"
                                  , Tuple "Authorization" $ "Token token=\"" <> token <> "\""
                                  ]
    options = FetchOptions.method := FetchOptions.POST
              <> FetchOptions.url := (url spotId)
              <> FetchOptions.headers := headers
              <> FetchOptions.body := (body detail)
