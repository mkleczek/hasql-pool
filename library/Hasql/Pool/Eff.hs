module Hasql.Pool.Eff (
    runWithPool,
) where

import Data.Functor ((<&>))
import Effectful (Eff, IOE, Subset, inject, raise, (:>))
import Effectful.Dispatch.Dynamic (interpret, localSeqLift, localSeqUnliftIO, localUnlift, reinterpret)
import Effectful.Error.Static (Error, catchError, runErrorNoCallStack, throwError)
import Hasql.Api.Eff.WithResource
import Hasql.Connection (Settings)
import Hasql.Pool (Pool, UsageError, use)
import qualified Hasql.Pool as P
import Hasql.Session
import Prelude

{-# INLINE runWithPoolH #-}
runWithPoolH :: (Error UsageError :> es, IOE :> es) => Pool -> (forall b. Eff les b -> Eff es b) -> Eff (WithConnection (Eff (Error QueryError : les)) : es) a -> Eff es a
runWithPoolH pool handler = interpret $ \env (WithResource action) -> do
    localSeqLift env $ \lift -> do
        result <- localSeqUnliftIO env $ \unlift ->
            use pool $ Session $ unlift . lift . handler . runErrorNoCallStack . action -- \eh -> handler $ ((runErrorNoCallStack . action) connection)
        either throwError pure result

{-# INLINEABLE runWithPool #-}
runWithPool :: forall es les a. (Error UsageError :> es, IOE :> es, Subset les es) => Pool -> Eff (WithConnection (Eff (Error QueryError : les)) : es) a -> Eff es a
runWithPool pool = runWithPoolH pool inject

dynamicPool :: (IOE :> es) => Int -> Maybe Int -> Settings -> Eff (DynamicResource Pool : es) a -> Eff es a
dynamicPool poolSize timeout settings = interpret $ \env -> \case
    Acquire -> localSeqUnliftIO env $ const $ P.acquire poolSize timeout settings
    Release pool -> localSeqUnliftIO env $ const $ P.release pool
