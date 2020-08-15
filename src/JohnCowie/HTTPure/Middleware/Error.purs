module JohnCowie.HTTPure.Middleware.Error where

import Prelude
import Data.Either (Either, either)

wrapHandleError :: forall m err req res. (Monad m) => (err -> m res) -> (req -> m (Either err res)) -> req -> m res
wrapHandleError errorHandler handler req = handler req >>= either errorHandler pure
