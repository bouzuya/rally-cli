module Request.CreateToken (createToken) where

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (runExcept)
import Data.CreateTokenResponse (CreateTokenResponse)
import Data.Either (either)
import Data.Foreign (F)
import Data.Foreign.Class (readJSON)
import Data.Options ((:=))
import Fetch (HTTP, fetch)
import Fetch.Options (Method(..), body, headers, method, url) as FetchOptions
import Prelude (($), (<>), (<<<), bind, show)
import Request.Helper.P (p, ps)
import Request.Helper.Headers (headersWithoutToken)

body :: String -> String -> String
body email password =
  ps [ p "view_type" "admin"
     , p "email" email
     , p "password" password
     ]

createToken :: forall eff
               . String
               -> String
               -> Aff ( http :: HTTP
                      | eff
                      ) CreateTokenResponse
createToken email password = do
  text <- fetch options
  let f = readJSON text :: F CreateTokenResponse
  makeAff (\ng ok -> either (ng <<< error <<< show) ok $ runExcept f)
  where
    options = FetchOptions.method := FetchOptions.POST
              <> FetchOptions.url := "https://api.rallyapp.jp/tokens"
              <> FetchOptions.headers := headersWithoutToken
              <> FetchOptions.body := (body email password)
