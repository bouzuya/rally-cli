module Request.CreateStampRally (createStampRally) where

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (runExcept)
import Data.CreateStampRallyResponse (CreateStampRallyResponse)
import Data.Either (either)
import Data.Foreign (F)
import Data.Foreign.Class (readJSON)
import Data.Options ((:=))
import Data.StrMap (fromFoldable) as StrMap
import Data.Tuple (Tuple(..))
import Fetch (HTTP, fetch)
import Fetch.Options (Method(..), body, headers, method, url) as FetchOptions
import Prelude (($), (<>), (<<<), bind, show)

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
    headers = StrMap.fromFoldable [ Tuple "Content-Type" "application/json"
                                  , Tuple "User-Agent" "rally-cli"
                                  , Tuple "Authorization" $ "Token token=\"" <> token <> "\""
                                  ]
    body =
      "{"
      <> " " <> show "view_type" <> ":" <> show "admin" <> ","
      <> " " <> show "display_name" <> ":" <> show displayName
      <> " }"
    options = FetchOptions.method := FetchOptions.POST
              <> FetchOptions.url := "https://api.rallyapp.jp/stamp_rallies"
              <> FetchOptions.headers := headers
              <> FetchOptions.body := body
