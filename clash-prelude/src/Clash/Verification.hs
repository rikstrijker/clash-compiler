{-|
Copyright  :  (C) 2019, Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Verification
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash    #-}
{-# LANGUAGE RankNTypes   #-}

module Clash.Verification where

import           Data.Text                         (Text)

import           Clash.Annotations.Primitive       (Primitive(InlinePrimitive), HDL(..))
import           Clash.Signal.Internal             (KnownDomain, Signal, Clock)
import           Clash.XException                  (errorX)

import           Clash.Verification.Types

-- | Convert a signal to a cv expression with a name hint. Clash will try its
-- best to use this name in the rendered assertion, but might run into
-- collisions.
name :: Text -> Signal dom Bool -> CvExpression dom
name nm signal = CvPure (Just nm, signal)
--{-# INLINE name #-}

lit :: Bool -> CvExpression dom
lit = CvLit
--{-# INLINE lit #-}

and :: (CvValue dom a, CvValue dom b) => a -> b -> CvExpression dom
and a b = CvAnd (toCvExpr a) (toCvExpr b)
--{-# INLINE and #-}

or :: (CvValue dom a, CvValue dom b) => a -> b -> CvExpression dom
or a b = CvOr (toCvExpr a) (toCvExpr b)
--{-# INLINE or #-}

implies :: (CvValue dom a, CvValue dom b) => a -> b -> CvExpression dom
implies a b = CvImplies (toCvExpr a) (toCvExpr b)
--{-# INLINE implies #-}

next :: CvValue dom a => a -> CvExpression dom
next = CvNext 1 . toCvExpr
--{-# INLINE next #-}

nextN :: CvValue dom a => Int -> a -> CvExpression dom
nextN n = CvNext n . toCvExpr
--{-# INLINE nextN #-}

after :: (CvValue dom a, CvValue dom b) => a -> b -> CvExpression dom
after a b = CvAfter (toCvExpr a) (toCvExpr b)
--{-# INLINE after #-}

timplies :: (CvValue dom a, CvValue dom b) => a -> b -> CvExpression dom
timplies a b = CvTemporalImplies 1 (toCvExpr a) (toCvExpr b)
--{-# INLINE timplies #-}

timpliesOverlapping :: (CvValue dom a, CvValue dom b) => a -> b -> CvExpression dom
timpliesOverlapping a b = CvTemporalImplies 0 (toCvExpr a) (toCvExpr b)
--{-# INLINE timpliesOverlapping #-}

always :: CvValue dom a => a -> CvProperty dom
always = CvAlways . toCvExpr
--{-# INLINE always #-}

never :: CvValue dom a => a -> CvProperty dom
never = CvNever . toCvExpr
--{-# INLINE never #-}

assert
  :: KnownDomain dom
  => Clock dom
  -> Text
  -- ^ Property name (used in reports and error messages)
  -> CvRenderAs
  -- ^ Assertion language to use in HDL
  -> CvProperty dom
  -> Signal dom CvResult
assert !_clk !_propName !_renderAs !_prop =
  pure (errorX "Simulation for Clash.Verification not yet implemented")
{-# NOINLINE assert #-}
{-# ANN assert (InlinePrimitive VHDL "[ { \"BlackBoxHaskell\" : { \"name\" : \"Clash.Verification.assert\", \"templateFunction\" : \"Clash.Primitives.Verification.assertBBF\"}} ]") #-}
{-# ANN assert (InlinePrimitive Verilog "[ { \"BlackBoxHaskell\" : { \"name\" : \"Clash.Verification.assert\", \"templateFunction\" : \"Clash.Primitives.Verification.assertBBF\"}} ]") #-}
{-# ANN assert (InlinePrimitive SystemVerilog "[ { \"BlackBoxHaskell\" : { \"name\" : \"Clash.Verification.assert\", \"templateFunction\" : \"Clash.Primitives.Verification.assertBBF\"}} ]") #-}

hideCvResult :: Signal dom CvResult -> a -> a
hideCvResult !_ a = a
{-# NOINLINE hideCvResult #-}

mergeResults :: Signal dom CvResult -> Signal dom CvResult -> Signal dom CvResult
mergeResults !_a !_b =
  pure (errorX "Simulation for Clash.Verification not yet implemented")
