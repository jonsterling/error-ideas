{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Control.Monad.Trace.ErrorTrace
( ErrorTrace(..)
, _ErrorTrace
, etError
, etTrace
) where

import Control.Applicative
import qualified Data.Foldable as F
import Data.Monoid
import Data.Profunctor
import Data.Sequence as S
import Data.List

-- | A datatype containing an error and its provenience(s).
--
data ErrorTrace t ft e
  = ErrorTrace
  { _etError ∷ !e -- ^ The error
  , _etTrace ∷ ft (Seq t) -- ^ The pointed monoid accumulating path traces
  }

instance (Monoid e, Monoid (ft (Seq t))) ⇒ Monoid (ErrorTrace t ft e) where
  mempty = ErrorTrace mempty mempty
  mappend (ErrorTrace e tr) (ErrorTrace e' tr') = ErrorTrace (e <> e') (tr <> tr')

instance (F.Foldable ft, Show t, Show e) ⇒ Show (ErrorTrace t ft e) where
  showsPrec p ErrorTrace{..} =
    showParen (p > 10) $
      foldr (.) id (intersperse (" ∥ "++) $ (foldr (.) id . intersperse ('.':) . fmap shows . F.toList) <$> F.toList _etTrace)
      . (" ⇑ " ++)
      . shows _etError

-- | An isomorphism @'ErrorTrace' t e ≅ (e, 'Seq' t)@.
--
_ErrorTrace
  ∷ ( Choice p
    , Functor f
    )
  ⇒ p (ErrorTrace t ft e) (f (ErrorTrace t' ft e'))
  → p (e, ft (Seq t)) (f (e', ft (Seq t')))
_ErrorTrace =
  dimap (uncurry ErrorTrace)
  . fmap $ \ErrorTrace{..} → (_etError, _etTrace)

-- | A lens @'ErrorTrace' t e → e@.
--
etError
  ∷ Functor f
  ⇒ (e → f e')
  → ErrorTrace t ft e
  → f (ErrorTrace t ft e')
etError inj ErrorTrace{..} =
  flip ErrorTrace _etTrace
    <$> inj _etError

-- | A lens @'ErrorTrace' t e → Seq t@.
--
etTrace
  ∷ Functor f
  ⇒ (ft (Seq t) → f (ft (Seq t')))
  → ErrorTrace t ft e
  → f (ErrorTrace t' ft e)
etTrace inj ErrorTrace{..} =
  ErrorTrace _etError
    <$> inj _etTrace
