module JohnCowie.HTTPure.Middleware.JSON
( JSONResponse
, okJsonResponse
, jsonResponse
, wrapJsonRequest
, wrapDecodeJson
, wrapJsonResponse
)
where

import Prelude

import Data.Argonaut.Core (Json, stringify) as JSON
import Data.Argonaut.Decode (class DecodeJson, decodeJson) as JSON
import Data.Argonaut.Encode (class EncodeJson, encodeJson) as JSON
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser) as JSON
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\), get1)
import HTTPure as HP
import JohnCowie.Data.Lens as L
import JohnCowie.HTTPure (class IsRequest, Response, response, addResponseHeader)
import JohnCowie.HTTPure as Req

type JSONResponse = Response JSON.Json

jsonResponse :: forall a. (JSON.EncodeJson a) => HP.Status -> a -> JSONResponse
jsonResponse status = response status <<< JSON.encodeJson

okJsonResponse :: forall a. (JSON.EncodeJson a) => a -> JSONResponse
okJsonResponse = jsonResponse 200

toJsonRequest :: forall req a.
                 (IsRequest req)
              => (Functor req)
              => req a
              -> Either String (req (Tuple JSON.Json a))
toJsonRequest req = do
  json <- JSON.jsonParser (L.view Req._body req)
  pure $ map (Tuple json) req

fromJsonResponse :: forall a. (JSON.EncodeJson a) => Response a -> Response String
fromJsonResponse {headers, status, body} =
  addResponseHeader "Content-Type" "application/json" $
  {headers, status, body: JSON.stringify $ JSON.encodeJson body}

wrapJsonRequest :: forall a req res.
                   (IsRequest req)
                => (Functor req)
                => (String -> res)
                -> (req (JSON.Json /\ a) -> res)
                -> req a
                -> res
wrapJsonRequest parseFail router req = case toJsonRequest req of
  (Left err) -> parseFail $ "ERROR: " <> err <> "\nBODY: " <> (L.view Req._body req)
  (Right jsonRequest) -> router jsonRequest

wrapDecodeJson :: forall a b req res.
                  (JSON.DecodeJson a)
               => (IsRequest req)
               => (Functor req)
               => (String -> res)
               -> (req (a /\ b) -> res)
               -> req (JSON.Json /\ b) -> res
wrapDecodeJson errorHandler router req = case JSON.decodeJson $ get1 (L.view Req._val req) of
  (Left err) -> errorHandler $ printJsonDecodeError err
  (Right updatedVal) -> router $ map (\(a /\ b) -> updatedVal /\ b) req

wrapJsonResponse :: forall a req m. (Bind m)
                 => (Applicative m)
                 => (JSON.EncodeJson a)
                 => (req -> m (Response a))
                 -> req
                 -> m (Response String)
wrapJsonResponse router request = do
  res <- router request
  pure $ fromJsonResponse res
