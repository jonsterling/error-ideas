{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module Control.Monad.Trans.FreeTrace
where

import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Operational
import Control.Monad.Identity
import Control.Monad.Trace.Class
import Control.Monad.Trace.ErrorTrace
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Either
import Control.Monad.Trans.State.Strict
import Data.Sequence as S hiding (singleton)

import Control.Monad.Reader

data TraceF t e m α where
  Catch ∷ TraceT t e m α → (e → TraceT t e m α) → TraceF t e m α
  Read ∷ TraceF t e m (Seq t)
  Scope ∷ t → TraceT t e m α → TraceF t e m α
  Throw ∷ e → TraceF t e m α

newtype TraceT t e m α = TraceT { _traceT ∷ ProgramT (TraceF t e m) m α }

deriving instance Monad m ⇒ Applicative (TraceT t e m)
deriving instance Monad m ⇒ Functor (TraceT t e m)
deriving instance Monad m ⇒ Monad (TraceT t e m)
deriving instance MonadIO m ⇒ MonadIO (TraceT t e m)
deriving instance MonadReader r m ⇒ MonadReader r (TraceT t e m)
deriving instance MonadState s m ⇒ MonadState s (TraceT t e m)

instance Monad m ⇒ MonadError e (TraceT t e m) where
  catchError m = TraceT . singleton . Catch m
  throwError = TraceT . singleton . Throw

instance MonadTrans (TraceT t e) where
  lift = TraceT . lift

instance Monad m ⇒ MonadTrace t (TraceT t e m) where
  traceScope t = TraceT . singleton . Scope t
  readTrace = TraceT $ singleton $ Read

runTraceT
  ∷ ( Functor m
    , Monad m
    )
  ⇒ TraceT t e m α
  → EitherT (ErrorTrace t e) m α
runTraceT = run S.empty . _traceT

run ∷ Monad m ⇒ Seq t → ProgramT (TraceF t e m) m α → EitherT (ErrorTrace t e) m α
run ts m = EitherT $ runEitherT . go ts =<< viewT m
  where
  go ∷ Monad m ⇒ Seq t → ProgramViewT (TraceF t e m) m α → EitherT (ErrorTrace t e) m α
  go ts (Catch m h :>>= k) = run ts (_traceT m) `catchError` (run ts . _traceT . h . _etError) >>= run ts . k
  go ts (Read :>>= k) = right ts >>= run ts . k
  go ts (Scope t m :>>= k) = run (ts |> t) (_traceT m) >>= run ts . k
  go ts (Throw e :>>= k) = left (ErrorTrace e ts) >>= run ts . k
  go ts (Return a) = right a
