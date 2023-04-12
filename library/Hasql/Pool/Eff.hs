module Hasql.Pool.Eff (
    runPoolHandler,
    runWithPoolError,
    runWithPoolEither,
) where

import Data.Functor ((<&>))
import Effectful (Eff, IOE, Subset, inject, raise, subsume, (:>))
import Effectful.Dispatch.Dynamic (interpret, localSeqLift, localSeqUnliftIO, localUnlift, reinterpret)
import Effectful.Error.Static (Error, catchError, runErrorNoCallStack, throwError)
import Hasql.Api.Eff.WithResource
import Hasql.Connection (Settings)
import Hasql.Pool (Pool, UsageError, use)
import qualified Hasql.Pool as P
import Hasql.Session
import Text.Read (Lexeme (String))
import Prelude

{-# INLINEABLE runPoolHandler #-}
runPoolHandler :: (Error UsageError :> es, IOE :> es) => Pool -> (forall b. Eff les b -> Eff es b) -> Eff (WithConnection (Eff (Error QueryError : les)) : es) a -> Eff es a
runPoolHandler pool handler = interpret $ \env (WithResource action) -> do
    localSeqLift env $ \lift -> do
        result <- localSeqUnliftIO env $ \unlift ->
            use pool $ Session $ unlift . lift . handler . runErrorNoCallStack . action
        either throwError pure result

{-# INLINE runWithPoolError #-}
runWithPoolError :: forall es les a. (Error UsageError :> es, IOE :> es, Subset les es) => Pool -> Eff (WithConnection (Eff (Error QueryError : les)) : es) a -> Eff es a
runWithPoolError pool = runPoolHandler pool inject

dynamicPool :: (IOE :> es) => Int -> Maybe Int -> Settings -> Eff (DynamicResource Pool : es) a -> Eff es a
dynamicPool poolSize timeout settings = interpret $ \env -> \case
    Acquire -> localSeqUnliftIO env $ const $ P.acquire poolSize timeout settings
    Release pool -> localSeqUnliftIO env $ const $ P.release pool

{-# INLINE runWithPoolEither #-}
runWithPoolEither :: forall es les a. (IOE :> es, Subset les es) => Pool -> Eff (WithConnection (Eff (Error QueryError : les)) : Error UsageError : es) a -> Eff es (Either UsageError a)
runWithPoolEither pool = runErrorNoCallStack . runPoolHandler pool (raise . inject)
