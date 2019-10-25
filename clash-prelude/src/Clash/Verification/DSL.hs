module Clash.Verification.DSL where

import qualified Clash.Verification           as Cv
import           Clash.Verification.Types

(&&) :: (CvValue dom a, CvValue dom b) => a -> b -> CvExpression dom
a && b = Cv.and a b
--{-# INLINE (&&) #-}

(||) :: (CvValue dom a, CvValue dom b) => a -> b -> CvExpression dom
a || b = Cv.or a b
--{-# INLINE (||) #-}

(~>) :: (CvValue dom a, CvValue dom b) => a -> b -> CvExpression dom
a ~> b = Cv.implies a b
--{-# INLINE (~>) #-}

(|=> ) :: (CvValue dom a, CvValue dom b) => a -> b -> CvExpression dom
a |=> b = Cv.timplies a b
--{-# INLINE (|=>) #-}

(|->) :: (CvValue dom a, CvValue dom b) => a -> b -> CvExpression dom
a |-> b = Cv.timpliesOverlapping a b
--{-# INLINE (|->) #-}

(##) :: (CvValue dom a, CvValue dom b) => a -> b -> CvExpression dom
a ## b = Cv.after a b
--{-# INLINE (>>) #-}
