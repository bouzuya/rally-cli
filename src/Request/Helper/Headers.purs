module Request.Helper.Headers
  ( headers
  , headersWithoutToken
  ) where

import Data.Show (show)
import Data.StrMap (StrMap, fromFoldable)
import Data.Tuple (Tuple(..))
import Prelude (($), (<>))

headers :: String -> StrMap String
headers token =
  fromFoldable [ Tuple "Content-Type" "application/json"
               , Tuple "User-Agent" "rally-cli"
               , Tuple "Authorization" $ "Token token=" <> show token
               ]

headersWithoutToken :: StrMap String
headersWithoutToken =
  fromFoldable [ Tuple "Content-Type" "application/json"
               , Tuple "User-Agent" "rally-cli"
               ]
