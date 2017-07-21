{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
-- | Defines the 'RIO' monad and some associated functions.
--
-- @since 0.1.0.0
module Control.Monad.RIO
  ( RIO (..)
  , runRIO
  , LogFunc
  , HasLogFunc (..)
  , HasStateRef (..)
  , HasWriterRef (..)
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.RWS.Class
import Control.Monad.Trans.Control
import Data.Monoid
import UnliftIO

-- | The Reader+IO monad. This is different from a 'ReaderT' because:
--
-- * It's not a transformer, it hardcodes IO for simpler usage and
-- error messages.
--
-- * Instances of typeclasses like 'MonadLogger' are implemented using
-- classes defined on the environment, instead of using an
-- underlying monad.
--
-- For much more information and the backstory on this, please see the
-- blog post <https://www.fpcomplete.com/blog/2017/07/the-rio-monad>.
--
-- @since 0.1.0.0
newtype RIO env a = RIO { unRIO :: ReaderT env IO a }
  deriving (Functor,Applicative,Monad,MonadIO,MonadReader env,MonadThrow,MonadCatch,MonadMask,MonadBase IO)

-- | Run a 'RIO' action with the given environment.
--
-- Note that, for convenience of use, we pass the environment first,
-- unlike similar functions like @runReaderT@.
--
-- @since 0.1.0.0
runRIO :: MonadIO m => env -> RIO env a -> m a
runRIO env (RIO (ReaderT f)) = liftIO (f env)

-- FIXME move into monad-logger itself
type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()
class HasLogFunc env where
  getLogFunc :: env -> LogFunc
instance HasLogFunc LogFunc where
  getLogFunc = id

instance HasLogFunc env => MonadLogger (RIO env) where
  monadLoggerLog a b c d = do
    f <- asks getLogFunc
    liftIO $ f a b c $ toLogStr d

instance HasLogFunc env => MonadLoggerIO (RIO env) where
  askLoggerIO = asks getLogFunc

instance MonadUnliftIO (RIO env) where
  askUnliftIO = RIO $ ReaderT $ \r ->
                withUnliftIO $ \u ->
                return (UnliftIO (unliftIO u . flip runReaderT r . unRIO))

instance MonadBaseControl IO (RIO env) where
  type StM (RIO env) a = a
  liftBaseWith f = RIO $ ReaderT $ \r -> f $ runRIO r
  restoreM = return

-- | An environment that contains a mutable reference for holding state.
--
-- @since 0.1.0.0
class HasStateRef s env | env -> s where
  getStateRef :: env -> IORef s -- TODO consider allowing something besides IORef
instance HasStateRef s (IORef s) where
  getStateRef = id

instance HasStateRef s env => MonadState s (RIO env) where
  put x = do
    ref <- asks getStateRef
    writeIORef ref $! x
  get = asks getStateRef >>= readIORef

-- | An environment that contains a mutable reference for a writer value.
--
-- @since 0.1.0.0
class Monoid w => HasWriterRef w env | env -> w where
  getWriterRef :: env -> IORef w -- TODO consider allowing something besides IORef
  setWriterRef :: IORef w -> env -> env
instance Monoid w => HasWriterRef w (IORef w) where
  getWriterRef = id
  setWriterRef ref _ = ref

instance HasWriterRef w env => MonadWriter w (RIO env) where
  writer (a, w) = do
    ref <- asks getWriterRef
    modifyIORef' ref (<> w)
    return a

  tell w = do
    ref <- asks getWriterRef
    modifyIORef' ref (<> w)

  listen f = do
    env <- ask
    ref <- newIORef mempty
    let env' = setWriterRef ref env
    a <- runRIO env' f
    w <- readIORef ref
    return (a, w)

  pass f = do
    (a, g) <- f
    ref <- asks getWriterRef
    modifyIORef' ref g
    return a

instance (HasWriterRef w env, HasStateRef s env) => MonadRWS env w s (RIO env) where

instance MonadError SomeException (RIO env) where -- Would be much nicer to have Exception e =>
  throwError = throwIO
  catchError = UnliftIO.catch
