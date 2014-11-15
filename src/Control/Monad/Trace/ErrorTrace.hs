{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
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
-- import Data.Pointed
import Data.Profunctor
import Data.Sequence as S
import Data.List

-- | A datatype containing an error and its provenience(s).
--
data ErrorTrace t e mt
  = ErrorTrace
  { _etError ∷ !e -- ^ The error
  , _etTrace ∷ mt (Seq t) -- ^ The pointed monoid accumulating path traces
  }

instance (Monoid e, Monoid (mt (Seq t))) ⇒ Monoid (ErrorTrace t e mt) where
  mempty = ErrorTrace mempty mempty
  mappend (ErrorTrace e tr) (ErrorTrace e' tr') = ErrorTrace (e <> e') (tr <> tr')

deriving instance (Show e, Show (mt (Seq t))) ⇒ Show (ErrorTrace t e mt)

-- instance (Show t, Show e) ⇒ Show (ErrorTrace t e mt) where
--   showsPrec p ErrorTrace{..} =
--     showParen (p > 10) $
--       foldr (.) id (intersperse (" ∥ "++) $ (foldr (.) id . intersperse ('.':) . fmap shows . F.toList) <$> _etTrace)
--       . (" ⇑ " ++)
--       . shows _etError

-- | An isomorphism @'ErrorTrace' t e ≅ (e, 'Seq' t)@.
--
_ErrorTrace
  ∷ ( Choice p
    , Functor f
    )
  ⇒ p (ErrorTrace t e mt) (f (ErrorTrace t' e' mt))
  → p (e, mt (Seq t)) (f (e', mt (Seq t')))
_ErrorTrace =
  dimap (uncurry ErrorTrace)
  . fmap $ \ErrorTrace{..} → (_etError, _etTrace)

-- | A lens @'ErrorTrace' t e → e@.
--
etError
  ∷ Functor f
  ⇒ (e → f e')
  → ErrorTrace t e mt
  → f (ErrorTrace t e' mt)
etError inj ErrorTrace{..} =
  flip ErrorTrace _etTrace
    <$> inj _etError

-- | A lens @'ErrorTrace' t e → Seq t@.
--
etTrace
  ∷ Functor f
  ⇒ (mt (Seq t) → f (mt (Seq t')))
  → ErrorTrace t e mt
  → f (ErrorTrace t' e mt)
etTrace inj ErrorTrace{..} =
  ErrorTrace _etError
    <$> inj _etTrace
