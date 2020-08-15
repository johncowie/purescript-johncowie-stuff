module JohnCowie.OAuth.Stub where

import Prelude
import Data.Either (Either(..))
import JohnCowie.OAuth (OAuth)

data StubCode = StubCode String

-- redirect :: RedirectBackTo -> String
-- , handleCode :: OAuthCode -> RedirectBackTo -> Aff (Either String UserData)

oauth :: String -> OAuth
oauth redirect = {
  redirect: redirect <> "?code=1234"
, handleCode: \code -> pure (Right { sub: "100"
                                   , name: "StubUser"
                                   , email: "stub@email.com"})
}
