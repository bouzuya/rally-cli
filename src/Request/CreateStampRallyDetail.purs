module Request.CreateStampRallyDetail (createStampRallyDetail) where

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (runExcept)
import Data.Detail (Detail(..))
import Data.Either (either)
import Data.Foreign (F)
import Data.Foreign.Class (readJSON)
import Data.Options ((:=))
import Fetch (HTTP, fetch)
import Fetch.Options (Method(..), body, headers, method, url) as FetchOptions
import Prelude (($), (<>), (<<<), bind, show)
import Request.Helper.P (p, ps)
import Request.Helper.Headers (headers)

body :: Detail -> String
body (Detail { name, value }) =
  ps [ p "view_type" "admin"
     , p "name" name
     , p "value" value
     ]

url :: String -> String
url stampRallyId =
  "https://api.rallyapp.jp/stamp_rallies/" <> stampRallyId <> "/details"

createStampRallyDetail
  :: forall eff
   . String
  -> Detail
  -> String
  -> Aff ( http :: HTTP
         | eff
         ) Detail
createStampRallyDetail stampRallyId detail token = do
  text <- fetch options
  let f = readJSON text :: F Detail
  makeAff (\ng ok -> either (ng <<< error <<< show) ok $ runExcept f)
  where
    options = FetchOptions.method := FetchOptions.POST
              <> FetchOptions.url := (url stampRallyId)
              <> FetchOptions.headers := (headers token)
              <> FetchOptions.body := (body detail)
