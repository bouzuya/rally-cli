module Request.CreateStampRally (createStampRally) where

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (runExcept)
import Data.CreateStampRallyResponse (CreateStampRallyResponse)
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
     , p "display_name" displayName
     ]

createStampRally
  :: forall eff
   . String
  -> String
  -> Aff ( http :: HTTP
         | eff
         ) CreateStampRallyResponse
createStampRally displayName token = do
  text <- fetch options
  let f = readJSON text :: F CreateStampRallyResponse
  makeAff (\ng ok -> either (ng <<< error <<< show) ok $ runExcept f)
  where
    options = FetchOptions.method := FetchOptions.POST
              <> FetchOptions.url := "https://api.rallyapp.jp/stamp_rallies"
              <> FetchOptions.headers := (headers token)
              <> FetchOptions.body := (body displayName)
