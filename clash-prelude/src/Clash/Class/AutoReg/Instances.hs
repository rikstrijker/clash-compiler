{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Clash.Class.AutoReg.Instances where

import Clash.Class.AutoReg.Internal
-- import Clash.CPP             (maxTupleSize)

#if MIN_VERSION_base(4,12,0)
import Data.Complex (Complex)
import Data.Ord (Down)
#endif

import Data.Ratio (Ratio)

#if MIN_VERSION_base(4,12,0)
deriveAutoReg ''Complex
deriveAutoReg ''Down
#endif

deriveAutoReg ''Ratio

-- TODO: this needs NFDataX instances to maxTupleSize
-- deriveAutoRegTuples [2..maxTupleSize]
deriveAutoRegTuples [2..15]
