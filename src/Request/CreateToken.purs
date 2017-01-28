module Request.CreateToken (createToken) where

import Control.Monad.Aff (Aff)
import Control.Monad.Except (runExcept)
import Data.CreateTokenResponse (CreateTokenResponse)
import Data.Either (fromRight)
import Data.Foreign (F)
import Data.Foreign.Class (readJSON)
import Data.Options ((:=))
import Data.StrMap (fromFoldable) as StrMap
import Data.Tuple (Tuple(..))
import Fetch (HTTP, fetch)
import Fetch.Options (Method(..), body, headers, method, url) as FetchOptions
import Partial.Unsafe (unsafePartial)
import Prelude (($), (<>), bind, pure)

createToken :: forall eff
               . String
               -> String
               -> Aff (http :: HTTP | eff) CreateTokenResponse
createToken email password = do
  text <- fetch options
  let e = runExcept $ readJSON text :: F CreateTokenResponse
  pure $ unsafePartial $ fromRight e
  where
    headers = StrMap.fromFoldable [ Tuple "Content-Type" "application/json"
                                  , Tuple "User-Agent" "rally-cli"
                                  ]
    body = "{" <>
          "\"view_type\":\"admin\"" <> "," <>
          "\"email\":\"" <> email <> "\"" <> "," <>
          "\"password\":\"" <> password <> "\"" <>
          "}"
    options = FetchOptions.method := FetchOptions.POST
              <> FetchOptions.url := "https://api.rallyapp.jp/tokens"
              <> FetchOptions.headers := headers
              <> FetchOptions.body := body
