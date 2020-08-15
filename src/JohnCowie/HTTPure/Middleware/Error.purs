module JohnCowie.HTTPure.Middleware.Error where

import Prelude
import Data.Either (Either, either)

wrapHandleError :: forall err req res. (err -> res) -> (req -> Either err res) -> (req -> res)
wrapHandleError errorHandler = map (either errorHandler identity)
