module Hasql.Pool.Eff (
    runWithPool,
) where

import Data.Functor ((<&>))
import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret, localSeqUnliftIO, localUnlift)
import Effectful.Error.Static (Error, throwError)
import Hasql.Api.Eff.WithResource
import Hasql.Pool (Pool, UsageError, use)
import Hasql.Session
import Prelude

{-# INLINEABLE runWithPool #-}
runWithPool :: (Error UsageError :> es, IOE :> es) => Pool -> Eff (WithConnection : es) a -> Eff es a
runWithPool pool = interpret $ \env (WithResource action) -> do
    result <- localSeqUnliftIO env $ \unlift ->
        use pool $ Session $ \connection ->
            unlift (action connection) <&> Right
    either throwError pure result
