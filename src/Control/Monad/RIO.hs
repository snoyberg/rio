{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
-- | Defines the 'RIO' monad and some associated functions.
--
-- @since 0.1.0.0
module Control.Monad.RIO
  ( RIO (..)
  , runRIO
  , LogFunc
  , HasLogFunc (..)
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
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
