module JohnCowie.OAuth.Google
( GoogleConfig
, oauth )
where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Affjax.RequestBody as RequestBody

import Control.Monad.Except.Trans (ExceptT(..), runExceptT)

import Data.Bifunctor (lmap)
import Data.Newtype (unwrap)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.FormURLEncoded (FormURLEncoded(..))
import Data.HTTP.Method (Method(POST))
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Error (printJsonDecodeError)

import URI.Extra.QueryPairs as Query
import URI.Query (print)

import Effect.Aff (Aff)

import JohnCowie.OAuth (OAuth, OAuthCode, UserData)
import JohnCowie.JWT (JWT, extractPayload)

import Envisage (Component, defaultTo, describe, mkComponent, showParsed, var)

-- mkRedirectUri ::  -> RedirectQuery -> String

queryString :: Array (Tuple String String) -> String
queryString pairs = print $
                    Query.print identity identity $
                    Query.QueryPairs $
                    map (\(Tuple k v) -> Tuple (Query.keyFromString k) (Just (Query.valueFromString v))) $
                    pairs

formData :: Array (Tuple String String) -> RequestBody.RequestBody
formData tuples = RequestBody.formURLEncoded $ FormURLEncoded $ map (map Just) tuples

{-
Redirect user to google, with query parameters set, e.g. clientID, callback url, etc..
-}
redirect :: GoogleConfig -> String -> String
redirect config redirectUri = config.oauthUrl <> query
  where query = queryString $
                [ Tuple "response_type" "code"
                , Tuple "access_type" "online"
                , Tuple "scope" "profile email"
                , Tuple "prompt" "select_account consent"
                , Tuple "client_id" config.clientId
                , Tuple "redirect_uri" redirectUri
                ]

fetchOpenIdData :: GoogleConfig -> String -> OAuthCode -> Aff (Either String {access_token :: JWT, id_token :: JWT})
fetchOpenIdData config redirectUri code = runExceptT $ do
  response <- ExceptT $ map (lmap AX.printError) $ AX.request request
  ExceptT $ pure $ lmap printJsonDecodeError $ decodeJson response.body
  where url = config.apiUrl <> "/oauth2/v4/token"
        body = formData [
          Tuple "code" (unwrap code)
        , Tuple "client_id" config.clientId
        , Tuple "client_secret" config.clientSecret
        , Tuple "redirect_uri" redirectUri
        , Tuple "grant_type" "authorization_code"
        ]
        request = AX.defaultRequest { responseFormat = ResponseFormat.json
                                    , method = Left POST
                                    , url = url
                                    , content = Just body}
        -- query =

{-
  code will come in query parameters
  receive the code from Google, and use to make request to fetch user data.

  response type of access code request:
  {access_token: <>
   id_token: JWT token - inside payload is sub (i.e. ID), name and email }

-}
handleCode :: GoogleConfig -> OAuthCode -> String -> Aff (Either String UserData)
handleCode config code redirectUri = runExceptT do
  tokenData <- ExceptT $ fetchOpenIdData config redirectUri code
  ExceptT $ pure $ extractPayload tokenData.id_token

type GoogleConfig = {
  oauthUrl :: String
, apiUrl :: String
, clientId :: String
, clientSecret :: String
}

oauth :: Component OAuth
oauth = mkComponent
  { oauthUrl: var "GOOGLE_OAUTH_URL" # defaultTo "https://accounts.google.com/o/oauth2/v2/auth" # showParsed
  , apiUrl: var "GOOGLE_API_URL" # defaultTo "https://www.googleapis.com" # showParsed
  , clientId: var "GOOGLE_CLIENT_ID" # describe "Client ID for google oauth account" # showParsed
  , clientSecret: var "GOOGLE_CLIENT_SECRET" # describe "Client secret for google oauth integration"}
  $ \config -> { redirect: redirect config
               , handleCode: handleCode config}
