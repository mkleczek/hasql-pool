module Hasql.Pool
  ( -- * Pool
    Pool,
    acquire,
    use,
    release,

    -- * Errors
    UsageError (..),
  )
where

import Data.UUID.V4 qualified as Uuid
import Hasql.Connection (Connection)
import Hasql.Connection qualified as Connection
import Hasql.Connection.Settings qualified as Connection.Settings
import Hasql.Errors qualified as Errors
import Hasql.Pool.Config.Config qualified as Config
import Hasql.Pool.Observation
import Hasql.Pool.Prelude
import Hasql.Pool.SessionErrorDestructors qualified as ErrorsDestruction
import Hasql.Session qualified as Session
import Pqi qualified

-- | A connection tagged with metadata.
data Entry = Entry
  { entryConnection :: Connection,
    entryCreationTimeNSec :: Word64,
    entryUseTimeNSec :: Word64,
    entryId :: UUID
  }

entryIsAged :: Word64 -> Word64 -> Entry -> Bool
entryIsAged maxLifetime now Entry {..} =
  now > entryCreationTimeNSec + maxLifetime

entryIsIdle :: Word64 -> Word64 -> Entry -> Bool
entryIsIdle maxIdletime now Entry {..} =
  now > entryUseTimeNSec + maxIdletime

-- | A pool request waiting for a connection.
data Waiter = Waiter
  { -- | What the pool request gets, with the reuse flag at the time: an idle
    -- connection, or the slot of a closed one to establish a connection in.
    waiterGiven :: TMVar (TVar Bool, Maybe Entry),
    -- | Whether the pool request stopped waiting, so that it gets passed over.
    waiterGone :: TVar Bool
  }

-- | Pool of connections to DB.
data Pool = Pool
  { -- | Pool size.
    poolSize :: Int,
    -- | Adapter used to establish connections.
    poolAdapter :: Pqi.Adapter,
    -- | Connection settings.
    poolFetchConnectionSettings :: IO Connection.Settings.Settings,
    -- | Acquisition timeout, in microseconds.
    poolAcquisitionTimeout :: Int,
    -- | Maximal connection lifetime, in nanoseconds.
    poolMaxLifetime :: Word64,
    -- | Maximal connection idle time, in nanoseconds.
    poolMaxIdletime :: Word64,
    -- | Avail connections.
    poolConnectionQueue :: TQueue Entry,
    -- | Remaining capacity.
    -- The pool size limits the sum of poolCapacity, the length
    -- of poolConnectionQueue and the number of in-flight
    -- connections.
    poolCapacity :: TVar Int,
    -- | Pool requests waiting for a connection, the longest waiting first.
    -- Freed connections and slots go to them before anyone else,
    -- so that only the pool request given one wakes up.
    poolWaiting :: TQueue Waiter,
    -- | Whether to return a connection to the pool.
    poolReuseVar :: TVar (TVar Bool),
    -- | To stop the manager thread via garbage collection.
    poolReaperRef :: IORef (),
    -- | Action for reporting the observations.
    poolObserver :: Observation -> IO (),
    -- | Initial session to execute upon every established connection.
    poolInitSession :: Session.Session ()
  }

-- | Create a connection-pool.
--
-- No connections actually get established by this function. It is delegated
-- to 'use'.
--
-- If you want to ensure that the pool connects fine at the initialization phase, just run 'use' with an empty session (@pure ()@) and check for errors.
--
-- The 'Pqi.Adapter' determines which connection implementation the pool uses, e.g. an FFI adapter backed by @postgresql-libpq@, or a pure Haskell one. Pick one from an adapter package such as @pqi-ffi@ or @pqi-native@.
acquire :: Pqi.Adapter -> Config.Config -> IO Pool
acquire adapter config = do
  connectionQueue <- newTQueueIO
  capVar <- newTVarIO (Config.size config)
  reuseVar <- newTVarIO =<< newTVarIO True
  waiting <- newTQueueIO
  reaperRef <- newIORef ()

  managerTid <- forkIOWithUnmask $ \unmask -> unmask $ forever $ do
    threadDelay 1000000
    now <- getMonotonicTimeNSec
    join . atomically $ do
      entries <- flushTQueue connectionQueue
      let (agedEntries, unagedEntries) = partition (entryIsAged agingTimeoutNanos now) entries
          (idleEntries, liveEntries) = partition (entryIsIdle maxIdletimeNanos now) unagedEntries
      traverse_ (writeTQueue connectionQueue) liveEntries
      return $ do
        forM_ agedEntries $ \entry -> do
          Connection.release (entryConnection entry)
          atomically $ freeSlot reuseVar waiting capVar
          (Config.observationHandler config) (ConnectionObservation (entryId entry) (TerminatedConnectionStatus AgingConnectionTerminationReason))
        forM_ idleEntries $ \entry -> do
          Connection.release (entryConnection entry)
          atomically $ freeSlot reuseVar waiting capVar
          (Config.observationHandler config) (ConnectionObservation (entryId entry) (TerminatedConnectionStatus IdlenessConnectionTerminationReason))

  void . mkWeakIORef reaperRef $ do
    -- When the pool goes out of scope, stop the manager.
    killThread managerTid

  return $ Pool (Config.size config) adapter (Config.connectionSettingsProvider config) acqTimeoutMicros agingTimeoutNanos maxIdletimeNanos connectionQueue capVar waiting reuseVar reaperRef (Config.observationHandler config) (Config.initSession config)
  where
    acqTimeoutMicros =
      div (fromIntegral (diffTimeToPicoseconds (Config.acquisitionTimeout config))) 1_000_000
    agingTimeoutNanos =
      div (fromIntegral (diffTimeToPicoseconds (Config.agingTimeout config))) 1_000
    maxIdletimeNanos =
      div (fromIntegral (diffTimeToPicoseconds (Config.idlenessTimeout config))) 1_000

-- | Release all the idle connections in the pool, and mark the in-use connections
-- to be released after use. Any connections acquired after the call will be
-- freshly established.
--
-- The pool remains usable after this action.
-- So you can use this function to reset the connections in the pool.
-- Naturally, you can also use it to release the resources.
release :: Pool -> IO ()
release Pool {..} =
  join . atomically $ do
    prevReuse <- readTVar poolReuseVar
    writeTVar prevReuse False
    newReuse <- newTVar True
    writeTVar poolReuseVar newReuse
    entries <- flushTQueue poolConnectionQueue
    return $ forM_ entries $ \entry -> do
      Connection.release (entryConnection entry)
      atomically $ freeSlot poolReuseVar poolWaiting poolCapacity
      poolObserver (ConnectionObservation (entryId entry) (TerminatedConnectionStatus ReleaseConnectionTerminationReason))

-- | Use a connection from the pool to run a session and return the connection
-- to the pool, when finished.
--
-- Session failing with a 'Session.ClientError' gets interpreted as a loss of
-- connection. In such case the connection does not get returned to the pool
-- and a slot gets freed up for a new connection to be established the next
-- time one is needed. The error still gets returned from this function.
--
-- __Warning:__ Due to the mechanism mentioned above you should avoid intercepting this error type from within sessions.
--
-- When no connection is available, the pool requests wait for one in the
-- order they came, and a later pool request doesn't take a connection while
-- others wait.
use :: Pool -> Session.Session a -> IO (Either UsageError a)
use Pool {..} sess =
  join . atomically $ do
    reuseVar <- readTVar poolReuseVar
    waited <- someoneWaiting poolWaiting
    -- Behind the pool requests already waiting, which get what's freed first.
    if waited
      then enqueue <&> onWaiter
      else
        asum
          [ readTQueue poolConnectionQueue <&> onConn reuseVar,
            do
              capVal <- readTVar poolCapacity
              if capVal > 0
                then do
                  writeTVar poolCapacity $! pred capVal
                  return $ onNewConn reuseVar
                else retry,
            enqueue <&> onWaiter
          ]
  where
    enqueue = do
      waiter <- Waiter <$> newEmptyTMVar <*> newTVar False
      writeTQueue poolWaiting waiter
      return waiter

    -- Only a pool request that waits registers a timer: GHC can't cancel one,
    -- so each stays until it fires.
    onWaiter waiter = do
      timedOut <- registerDelay poolAcquisitionTimeout
      given <-
        atomically
          ( (Just <$> takeTMVar (waiterGiven waiter))
              <|> do
                readTVar timedOut >>= check
                writeTVar (waiterGone waiter) True
                return Nothing
          )
          `onException` abandon waiter
      case given of
        Nothing -> return $ Left AcquisitionTimeoutUsageError
        Just (reuseVar, Nothing) -> onNewConn reuseVar
        Just (reuseVar, Just entry) -> onConn reuseVar entry

    -- A pool request interrupted while waiting gives back what it was given
    -- meanwhile.
    abandon waiter = do
      given <- atomically $ do
        writeTVar (waiterGone waiter) True
        tryTakeTMVar (waiterGiven waiter)
      forM_ given \case
        (_, Nothing) -> atomically returnSlot
        (reuseVar, Just entry) -> returnConn reuseVar entry

    returnSlot = freeSlot poolReuseVar poolWaiting poolCapacity

    onNewConn reuseVar = do
      settings <- poolFetchConnectionSettings
      now <- getMonotonicTimeNSec
      id <- Uuid.nextRandom
      poolObserver (ConnectionObservation id ConnectingConnectionStatus)
      Connection.acquire poolAdapter settings >>= \case
        Left connErr -> do
          let connErrText = case connErr of
                Errors.NetworkingConnectionError details -> Just details
                Errors.AuthenticationConnectionError details -> Just details
                Errors.CompatibilityConnectionError details -> Just details
                Errors.OtherConnectionError details -> if details == "" then Nothing else Just details
          poolObserver (ConnectionObservation id (TerminatedConnectionStatus (NetworkErrorConnectionTerminationReason connErrText)))
          atomically returnSlot
          return $ Left $ ConnectionUsageError connErr
        Right connection -> do
          Connection.use connection poolInitSession >>= \case
            Left err -> do
              Connection.release connection
              atomically returnSlot
              ErrorsDestruction.reset
                ( \details -> do
                    poolObserver (ConnectionObservation id (TerminatedConnectionStatus (NetworkErrorConnectionTerminationReason (Just details))))
                )
                (poolObserver (ConnectionObservation id (TerminatedConnectionStatus (InitializationErrorTerminationReason err))))
                err
              return $ Left $ SessionUsageError err
            Right () -> do
              poolObserver (ConnectionObservation id (ReadyForUseConnectionStatus EstablishedConnectionReadyForUseReason))
              onLiveConn reuseVar (Entry connection now now id)

    onConn reuseVar entry = do
      now <- getMonotonicTimeNSec
      if entryIsAged poolMaxLifetime now entry
        then do
          Connection.release (entryConnection entry)
          poolObserver (ConnectionObservation (entryId entry) (TerminatedConnectionStatus AgingConnectionTerminationReason))
          onNewConn reuseVar
        else
          if entryIsIdle poolMaxIdletime now entry
            then do
              Connection.release (entryConnection entry)
              poolObserver (ConnectionObservation (entryId entry) (TerminatedConnectionStatus IdlenessConnectionTerminationReason))
              onNewConn reuseVar
            else do
              onLiveConn reuseVar entry {entryUseTimeNSec = now}

    onLiveConn reuseVar entry = do
      poolObserver (ConnectionObservation (entryId entry) InUseConnectionStatus)
      sessRes <- try @SomeException (Connection.use (entryConnection entry) sess)

      case sessRes of
        Left exc -> do
          returnConn reuseVar entry
          throwIO exc
        Right (Left err) ->
          if ErrorsDestruction.requiresConnectionDiscard err
            then do
              Connection.release (entryConnection entry)
              atomically returnSlot
              poolObserver
                ( ConnectionObservation
                    (entryId entry)
                    (TerminatedConnectionStatus (NetworkErrorConnectionTerminationReason (ErrorsDestruction.discardDetails err)))
                )
              return $ Left $ SessionUsageError err
            else do
              returnConn reuseVar entry
              poolObserver (ConnectionObservation (entryId entry) (ReadyForUseConnectionStatus (SessionFailedConnectionReadyForUseReason err)))
              return $ Left $ SessionUsageError err
        Right (Right res) -> do
          returnConn reuseVar entry
          poolObserver (ConnectionObservation (entryId entry) (ReadyForUseConnectionStatus SessionSucceededConnectionReadyForUseReason))
          return $ Right res

    -- Back to the pool request waiting longest or to the idle connections,
    -- unless the pool was released meanwhile.
    returnConn reuseVar entry =
      join . atomically $ do
        reuse <- readTVar reuseVar
        if reuse
          then do
            nextWaiter poolWaiting >>= \case
              Just waiter -> do
                currentReuseVar <- readTVar poolReuseVar
                putTMVar (waiterGiven waiter) (currentReuseVar, Just entry)
              Nothing -> writeTQueue poolConnectionQueue entry
            return $ return ()
          else return $ do
            Connection.release (entryConnection entry)
            atomically returnSlot
            poolObserver (ConnectionObservation (entryId entry) (TerminatedConnectionStatus ReleaseConnectionTerminationReason))

-- | Whether a pool request is waiting. The ones that stopped waiting get
-- dropped.
someoneWaiting :: TQueue Waiter -> STM Bool
someoneWaiting waiting =
  tryPeekTQueue waiting >>= \case
    Nothing -> return False
    Just waiter ->
      readTVar (waiterGone waiter) >>= \case
        True -> readTQueue waiting *> someoneWaiting waiting
        False -> return True

-- | Take the pool request waiting longest, passing over the ones that stopped
-- waiting.
nextWaiter :: TQueue Waiter -> STM (Maybe Waiter)
nextWaiter waiting =
  tryReadTQueue waiting >>= \case
    Nothing -> return Nothing
    Just waiter ->
      readTVar (waiterGone waiter) >>= \case
        True -> nextWaiter waiting
        False -> return (Just waiter)

-- | Give the slot of a closed connection to the pool request waiting longest,
-- or back to the pool if no pool request waits.
freeSlot :: TVar (TVar Bool) -> TQueue Waiter -> TVar Int -> STM ()
freeSlot reuseVarVar waiting capVar =
  nextWaiter waiting >>= \case
    Just waiter -> do
      reuseVar <- readTVar reuseVarVar
      putTMVar (waiterGiven waiter) (reuseVar, Nothing)
    Nothing -> modifyTVar' capVar succ

-- | Union over all errors that 'use' can result in.
data UsageError
  = -- | Attempt to establish a connection failed.
    ConnectionUsageError Errors.ConnectionError
  | -- | Session execution failed.
    SessionUsageError Errors.SessionError
  | -- | Timeout acquiring a connection.
    AcquisitionTimeoutUsageError
  deriving (Show, Eq)

instance Exception UsageError
