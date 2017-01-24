module Data.CreateTokenResponse where

import Data.Foreign.Class (class IsForeign, readProp)
import Data.Show (class Show)
import Prelude (($), (<>), bind, pure)

data CreateTokenResponse = CreateTokenResponse
  { token :: String
  , refreshToken :: String
  , expiredAt :: String
  , refreshTokenExpiredAt :: String
  , userId :: String
  }

instance createTokenResponseShow :: Show CreateTokenResponse where
  show (CreateTokenResponse { token
                            , refreshToken
                            , expiredAt
                            , refreshTokenExpiredAt
                            , userId
                            }) =
    "(CreateTokenResponse { "
    <> "token: \"" <> token <> "\", "
    <> "refreshToken: \"" <> refreshToken <> "\", "
    <> "expiredAt: \"" <> expiredAt <> "\", "
    <> "refreshTokenExpiredAt: \"" <> refreshTokenExpiredAt <> "\", "
    <> "userId: \"" <> userId <> "\" }"

instance createTokenResponseIsForeign :: IsForeign CreateTokenResponse where
  read value = do
    token <- readProp "token" value
    refreshToken <- readProp "refreshToken" value
    expiredAt <- readProp "expiredAt" value
    refreshTokenExpiredAt <- readProp "refreshTokenExpiredAt" value
    userId <- readProp "userId" value
    pure $ CreateTokenResponse { token
                               , refreshToken
                               , expiredAt
                               , refreshTokenExpiredAt
                               , userId
                               }
