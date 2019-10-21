{- # LANGUAGE UndecidableInstances #-}
module AutoRegTest2 where
-- import AutoReg
-- import AutoRegDeriving
import Clash.Prelude

data Pair aa bb = MkPair { getA :: aa, getB :: bb } deriving (Generic,NFDataX)
deriveAutoReg ''Pair

-- arbitrary constraints, these get copied to derived instances for types containing Pair
-- instance (Ord a, Eq b, NFDataX a, NFDataX b, BitPack a, KnownNat (BitSize b)) => AutoReg (Pair a b) where
--   autoReg' = undefined


data Tup3 a b c = MkTup3 { getAB :: Pair a b, getC :: c } deriving (Generic,NFDataX)

newtype PairAndNat a b (c::Nat) = Pair2 (Pair a b) deriving (Generic,NFDataX)

data Tup3' a b c d = MkTup3' (PairAndNat b b d) c deriving (Generic,NFDataX)

newtype BoolAnd a = BoolAnd (Pair a Bool) deriving (Generic,NFDataX)

data AandMaybeA a = AandMaybeA a (Maybe a) deriving (Generic,NFDataX)

newtype AandMaybeA2 a = AandMaybeA2 (Pair a (Maybe a)) deriving (Generic,NFDataX)

newtype AandMaybeB a b = AandMaybeB (Pair a (Maybe b)) deriving (Generic,NFDataX)


deriveAutoReg ''Tup3
deriveAutoReg ''PairAndNat
deriveAutoReg ''Tup3'
deriveAutoReg ''BoolAnd
deriveAutoReg ''AandMaybeA
deriveAutoReg ''AandMaybeA2
deriveAutoReg ''AandMaybeB
