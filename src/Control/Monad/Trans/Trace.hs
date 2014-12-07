{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module Control.Monad.Trans.Trace
( TraceT
, runTraceT
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Error.Class
import Control.Monad.Identity
import Control.Monad.Trace.Class
import Control.Monad.Trace.ErrorTrace
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Either
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Data.Monoid
import Data.Pointed
import Data.Sequence as S

-- | A concrete monad transformer @'TraceT' t e m@ where @t@ is the type of
-- tags/breadcrumbs, @e@ is the type of errors, and @m@ is the underlying monad.
--
newtype TraceT t ft e m α
  = TraceT
  { _traceT ∷ EitherT (ErrorTrace t ft e) (ReaderT (Seq t) m) α
  } deriving (Functor, Monad, Applicative, MonadIO, MonadBase b)

deriving instance
  ( Monad m
  , Monoid e
  , Monoid (ft (Seq t))
  ) ⇒ Alternative (TraceT t ft e m)

instance (Monad m, Pointed ft) ⇒ MonadError e (TraceT t ft e m) where
  throwError e = readTrace >>= TraceT . left . ErrorTrace e . point
  catchError (TraceT m) h = TraceT (lift $ runEitherT m) >>= either (h . _etError) return

instance MonadTrans (TraceT t ft e) where
  lift = TraceT . EitherT . (>>= return . Right) . lift

instance Monad m ⇒ MonadTrace t (TraceT t ft e m) where
  traceScope t = TraceT . mapEitherT (withReaderT (|> t)) . _traceT
  readTrace = TraceT . EitherT $ ask >>= return . Right

-- | Run a traced traced computation to get either its result, or an error and
-- its provenience ('ErrorTrace').
--
runTraceT
  ∷ ( Functor m
    , Monad m
    )
  ⇒ TraceT t ft e m α
  → m (Either (ErrorTrace t ft e) α)
runTraceT (TraceT m) = runReaderT (runEitherT m) S.empty

instance MonadTransControl (TraceT t ft e) where
  newtype StT (TraceT t ft e) α = StTraceT { unStTraceT ∷ StT (ReaderT (Seq t)) (StT (EitherT (ErrorTrace t ft e)) α) }
  liftWith f = TraceT . liftWith $ \run → liftWith $ \run' → f $ liftM StTraceT . run' . run . _traceT
  {-# INLINE liftWith #-}
  restoreT = TraceT . restoreT . restoreT . liftM unStTraceT
  {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (TraceT t ft e m) where
  newtype StM (TraceT t ft e m) α = StMTraceT { unStMTraceT ∷ ComposeSt (TraceT t ft e) m α }
  liftBaseWith = defaultLiftBaseWith StMTraceT
  {-# INLINE liftBaseWith #-}
  restoreM  = defaultRestoreM unStMTraceT
  {-# INLINE restoreM #-}
