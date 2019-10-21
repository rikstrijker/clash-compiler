{-# LANGUAGE DataKinds,TypeApplications,TemplateHaskell #-}
{-# LANGUAGE GADTs,StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module AutoRegTest where
import Clash.Explicit.Prelude

-- import AutoReg
-- import AutoRegDeriving


data MyPair a b = MkPair { getA :: a, getB :: b } deriving (Generic,NFDataX)
-- deriveAutoReg ''MyPair
deriveAutoReg ''MyPair

instance (BitPack a, BitPack b, KnownNat (BitSize a), KnownNat (BitSize b)) => BitPack (MyPair a b)

data BAndI = BAndI Bool Int deriving (Generic,NFDataX,BitPack)
-- instance (AutoReg a, Eq a, NFDataX b, Ord b) => AutoReg (MyPair a b) where
--   autoReg' = error "todo"


newtype MyPair' c d = MyPair' {unPair' :: MyPair c d }  deriving (Generic,NFDataX)
-- deriveAutoReg ''MyPair'


-- instance (AutoReg a, Eq a, NFDataX b, Ord b) => AutoReg (MyPair' a b) where
--   autoReg clk rst en initVal m = MyPair' <$> autoReg clk rst en (unPair' initVal) (fmap unPair' m)


data Tup3 a b c = MkTup3 { fieldA :: a, fieldB :: b, fieldC :: c } deriving (Generic,NFDataX)
-- deriveAutoReg ''Tup3


newtype OtherPair a b = OtherPair (MyPair a b) deriving (Generic,NFDataX)
-- deriveAutoReg ''OtherPair

data MyPair2 a b c = MkPair2 a b b deriving (Generic,NFDataX)
-- deriveAutoReg ''MyPair2

data Concrete = BoolAndInt Bool Int deriving (Generic,NFDataX)
-- deriveAutoReg ''Concrete

-- deriveAutoReg ''(,)
-- deriveAutoReg ''(,,)


data InfixDataCon a b = a :-.- b deriving (Generic,NFDataX)
-- deriveAutoReg ''InfixDataCon


data TestNoFields = NoFields
-- deriveAutoReg ''TestNoFields

-- data Foo a = forall b. MkFoo a b
-- deriving instance Generic (Foo a)
-- -- ,NFDataX)




-- topEntity clk rst =
--   -- autoReg @(Maybe (Unsigned 4,BitVector 6)) @System clk rst enableGen (Just (3,5))
--   -- autoReg @(Unsigned 3,Bool,Unsigned 5) @System clk rst enableGen (2,False,3)
--   -- autoReg @(Vec 3 (Bool,Unsigned 4)) @System clk rst enableGen (((,) False) <$> 2 :> 3:>4:> Nil)
--   -- autoReg @(Concrete) @System clk rst enableGen (BoolAndInt True 1)
--   autoReg @(MyPair (Unsigned 3) Bool) @System clk rst enableGen (MkPair 2 False)
--   -- autoRegGeneric @(MyPair (Unsigned 3) Bool) @System clk rst enableGen (MkPair 2 False)

-- initVal :: (MyPair (Tup3 Bool (Unsigned 3) (BitVector 4))  (Tup3 Bool (Unsigned 3) (BitVector 4)) )
-- initVal = MkPair a a where a = (MkTup3 False 2 3)
-- initVal :: Tup3 Bool (Unsigned 3) (BitVector 4)
-- initVal = a where a = (MkTup3 False 2 3)
-- initVal = let a = 2 :: Unsigned 3 in a :> a:> a:>a :>Nil
initVal = Just $ MkPair True False
-- initVal = MyPair' (MkPair True False)
-- initVal = Just True

testAutoGeneric,testAutoTH :: Clock System -> Reset System
  -- -> Signal System (Tup3 Bool (Unsigned 3) (BitVector 4))
  -- -> Signal System _ (MyPair (Tup3 Bool (Unsigned 3) (BitVector 4))  (Tup3 Bool (Unsigned 3) (BitVector 4)) )
  -> _
testAutoGeneric clk rst = undefined -- autoRegGeneric clk rst enableGen initVal
testAutoTH clk rst =  autoReg clk rst enableGen initVal
{- # ANN testAutoGeneric (defSyn "testAutoGeneric") #-}
{-# ANN testAutoTH (defSyn "testAutoTH") #-}

-- topEntity =
--   testAutoTH
--   -- testAutoGeneric
