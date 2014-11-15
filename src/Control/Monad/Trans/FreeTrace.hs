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
( TraceT
, runTraceT
) where

import Control.Applicative as A
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
import Data.Pointed
import Data.Sequence as S hiding (singleton)

import Control.Monad.Reader

data TraceF t e (mt ∷ * → *) m α where
  Catch ∷ TraceT t e mt m α → (e → TraceT t e mt m α) → TraceF t e mt m α
  Read ∷ TraceF t e mt m (Seq t)
  Scope ∷ t → TraceT t e mt m α → TraceF t e mt m α
  Throw ∷ e → TraceF t e mt m α

newtype TraceT t e mt m α = TraceT { _traceT ∷ ProgramT (TraceF t e mt m) m α }

unViewT
  ∷ Monad m
  ⇒ m (ProgramViewT instr m α)
  → ProgramT instr m α
unViewT = join . lift . (return . go =<<)
  where
    go (m :>>= k) = singleton m >>= k
    go (Return a) = return a

instance (Alternative m, Monad m) ⇒ Alternative (ProgramT instr m) where
  empty = lift A.empty
  m1 <|> m2 =  unViewT (viewT m1 <|> viewT m2)

deriving instance (Alternative m, Monad m) ⇒ Alternative (TraceT t e mt m)
deriving instance Monad m ⇒ Applicative (TraceT t e mt m)
deriving instance Monad m ⇒ Functor (TraceT t e mt m)
deriving instance Monad m ⇒ Monad (TraceT t e mt m)
deriving instance MonadIO m ⇒ MonadIO (TraceT t e mt m)
deriving instance MonadReader r m ⇒ MonadReader r (TraceT t e mt m)
deriving instance MonadState s m ⇒ MonadState s (TraceT t e mt m)

instance Monad m ⇒ MonadError e (TraceT t e mt m) where
  catchError m = TraceT . singleton . Catch m
  throwError = TraceT . singleton . Throw

instance MonadTrans (TraceT t e mt) where
  lift = TraceT . lift

instance Monad m ⇒ MonadTrace t (TraceT t e mt m) where
  traceScope t = TraceT . singleton . Scope t
  readTrace = TraceT $ singleton $ Read

runTraceT
  ∷ ( Functor m
    , Monad m
    , Pointed mt
    )
  ⇒ TraceT t e mt m α
  → EitherT (ErrorTrace t e mt) m α
runTraceT = run S.empty . _traceT

run
  ∷ ( Monad m
    , Pointed mt
    )
  ⇒ Seq t
  → ProgramT (TraceF t e mt m) m α
  → EitherT (ErrorTrace t e mt) m α
run ts m = EitherT $ runEitherT . go ts =<< viewT m
  where
    go ts (Catch m h :>>= k) = return (run ts (_traceT m)) >>= (flip catchError (run ts . _traceT . h . _etError) >=> run ts . k)
    go ts (Read :>>= k) = right ts >>= run ts . k
    go ts (Scope t m :>>= k) = run (ts |> t) (_traceT m) >>= run ts . k
    go ts (Throw e :>>= k) = left (ErrorTrace e $ point ts) >>= run ts . k
    go ts (Return a) = right a
