module Hasql.Pool.Eff (
    runPoolHandler,
    runPoolHandler',
    runWithPoolError,
    runWithPoolEither,
    LocalEffectHandler (..),
    PoolThrowError (..),
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

class LocalEffectHandler locales es where
    handleLocal :: forall a. Eff locales a -> Eff es a

instance LocalEffectHandler es es where
    handleLocal = id

class PoolThrowError e es where
    poolThrowError :: e -> Eff es a

{-# INLINEABLE runPoolHandler' #-}
runPoolHandler' :: (IOE :> es) => Pool -> (forall b. Eff les b -> Eff es b) -> (forall b. UsageError -> Eff es b) -> Eff (WithConnection (Eff (Error QueryError : les)) : es) a -> Eff es a
runPoolHandler' pool handler throwError = interpret $ \env (WithResource action) -> do
    localSeqLift env $ \lift -> do
        result <- localSeqUnliftIO env $ \unlift ->
            use pool $ Session $ unlift . lift . handler . runErrorNoCallStack . action
        either throwError pure result

{-# INLINE runPoolHandler #-}
runPoolHandler :: (LocalEffectHandler les es, PoolThrowError UsageError es, IOE :> es) => Pool -> Eff (WithConnection (Eff (Error QueryError : les)) : es) a -> Eff es a
runPoolHandler pool = runPoolHandler' pool handleLocal poolThrowError

{-# INLINE runWithPoolError #-}
runWithPoolError :: forall es les a. (Error UsageError :> es, IOE :> es, Subset les es) => Pool -> Eff (WithConnection (Eff (Error QueryError : les)) : es) a -> Eff es a
runWithPoolError pool = runPoolHandler' pool inject throwError

dynamicPool :: (IOE :> es) => Int -> Maybe Int -> Settings -> Eff (DynamicResource Pool : es) a -> Eff es a
dynamicPool poolSize timeout settings = interpret $ \env -> \case
    Acquire -> localSeqUnliftIO env $ const $ P.acquire poolSize timeout settings
    Release pool -> localSeqUnliftIO env $ const $ P.release pool

{-# INLINE runWithPoolEither #-}
runWithPoolEither :: forall es les a. (IOE :> es, Subset les (Error UsageError : es)) => Pool -> Eff (WithConnection (Eff (Error QueryError : les)) : Error UsageError : es) a -> Eff es (Either UsageError a)
runWithPoolEither pool = runErrorNoCallStack . runPoolHandler' pool inject throwError
