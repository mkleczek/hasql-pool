module Hasql.Pool.Eff (
    runWithPool,
) where

import Data.Functor ((<&>))
import Effectful (Eff, IOE, raise, (:>))
import Effectful.Dispatch.Dynamic (interpret, localSeqUnliftIO, localUnlift)
import Effectful.Error.Static (Error, catchError, runErrorNoCallStack, throwError)
import Hasql.Api.Eff.WithResource
import Hasql.Connection (Settings)
import Hasql.Pool (Pool, UsageError, use)
import qualified Hasql.Pool as P
import Hasql.Session
import Prelude

{-# INLINEABLE runWithPool #-}
runWithPool :: forall es a. (Error UsageError :> es, IOE :> es) => Pool -> Eff (WithConnection : es) a -> Eff es a
runWithPool pool = interpret $ \env (WithResource action) -> do
    result <- localSeqUnliftIO env $ \unlift ->
        use pool $ Session $ \connection -> unlift $ (runErrorNoCallStack . raise . action) connection
    either throwError pure result

dynamicPool :: (IOE :> es) => Int -> Maybe Int -> Settings -> Eff (DynamicResource Pool : es) a -> Eff es a
dynamicPool poolSize timeout settings = interpret $ \env -> \case
    Acquire -> localSeqUnliftIO env $ const $ P.acquire poolSize timeout settings
    Release pool -> localSeqUnliftIO env $ const $ P.release pool
