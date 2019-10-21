{-# LANGUAGE DataKinds,TypeOperators,TypeFamilies,TypeApplications,FlexibleContexts,TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}  -- needed for constaint on the Fixed instance
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ViewPatterns #-}
module Clash.Class.AutoReg.Internal where
import Data.List (nub,zipWith4)
import Data.Maybe (fromMaybe,isJust)

import GHC.TypeNats (KnownNat,Nat,type (+))
import Clash.Explicit.Signal
import Clash.Promoted.Nat
import Clash.Magic
import Clash.XException (NFDataX, deepErrorX)

import Clash.Sized.BitVector
import Clash.Sized.Fixed
import Clash.Sized.Index
import Clash.Sized.RTree
import Clash.Sized.Signed
import Clash.Sized.Unsigned
import Clash.Sized.Vector (Vec, lazyV, smap)

import Data.Int
import Data.Word
import Foreign.C.Types                (CUShort)
import Numeric.Half                   (Half)

import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Ppr

import Control.Lens.Internal.TH (bndrName,conAppsT,unfoldType)


class (NFDataX a) => AutoReg a where
  autoReg'
    :: KnownDomain dom
    => Bool
    -> Clock dom -> Reset dom -> Enable dom
    -> a -- ^ Reset value
    -> Signal dom a -> Signal dom a
  autoReg' = const register

autoReg,autoReg1
  :: (KnownDomain dom, AutoReg a)
  => Clock dom -> Reset dom -> Enable dom
  -> a -- ^ Reset value
  -> Signal dom a -> Signal dom a
autoReg = autoReg' True
autoReg1 = autoReg' False

instance AutoReg ()
instance AutoReg Bool

instance AutoReg Double
instance AutoReg Float
instance AutoReg CUShort
instance AutoReg Half

instance AutoReg Char

instance AutoReg Integer
instance AutoReg Int
instance AutoReg Int8
instance AutoReg Int16
instance AutoReg Int32
instance AutoReg Int64
instance AutoReg Word
instance AutoReg Word8
instance AutoReg Word16
instance AutoReg Word32
instance AutoReg Word64

instance AutoReg Bit
instance AutoReg (BitVector n)
instance AutoReg (Signed n)
instance AutoReg (Unsigned n)
instance AutoReg (Index n)
instance NFDataX (rep (int + frac)) => AutoReg (Fixed rep int frac)

instance AutoReg a => AutoReg (Maybe a) where
  autoReg' autoDeep clk rst en initVal input =
    let
        tag = isJust <$> input
        tagInit = isJust initVal
        tagR = register clk rst en tagInit tag

        val = fromMaybe (deepErrorX "undefined") <$> input
        valInit = fromMaybe (deepErrorX "undefined") initVal

        reg | autoDeep  = autoReg
            | otherwise = register
        valR = reg clk rst (toEnable (fromEnable en .&&. tag)) valInit val

        createMaybe t v = case t of
          True -> Just v
          False -> Nothing

    in createMaybe <$> tagR <*> valR

instance (KnownNat n, AutoReg a) => AutoReg (Vec n a) where
  autoReg' :: forall dom. KnownDomain dom
          => Bool
          -> Clock dom -> Reset dom -> Enable dom
          -> Vec n a -- ^ Reset value
          -> Signal dom (Vec n a) -> Signal dom (Vec n a)
  autoReg' autoDeep clk rst en initVal xs =
    bundle $ smap go (lazyV initVal) <*> unbundle xs
    where
      reg | autoDeep  = autoReg
          | otherwise = register
      go :: forall (i :: Nat). SNat i -> a  -> Signal dom a -> Signal dom a
      go SNat = suffixNameFromNat @i . reg clk rst en

instance (KnownNat d, AutoReg a) => AutoReg (RTree d a) where
  autoReg' autoDeep clk rst en initVal xs =
    bundle $ (reg clk rst en) <$> lazyT initVal <*> unbundle xs
    where
      reg | autoDeep  = autoReg
          | otherwise = register




-- | Automaticaly derives an 'AutoReg' instance for a product type
--
-- Usage:
-- > data Pair a b = MkPair { getA :: a, getB :: b } deriving (Generic,NFDataX)
-- > data Tup3 a b c = MkTup3 { getAB :: Pair a b, getC :: c } deriving (Generic,NFDataX)
-- > deriveAutoReg ''Pair
-- > deriveAutoReg ''Tup3
--
-- __NB__: Because of the way template haskell works the order here matters,
-- if you try to @deriveAutoReg ''Tup3@ before @Pair@ it will complain
-- about missing an @instance AutoReg (Pair a b)@.
deriveAutoReg :: Name -> DecsQ
deriveAutoReg tyNm = do
  tyInfo <- reifyDatatype tyNm
  case datatypeCons tyInfo of
    [] -> fail $ "Can't deriveAutoReg for empty types"
    [conInfo] -> deriveAutoRegProduct tyInfo conInfo
    _ -> fail "Can't deriveAutoReg for sum types"



{-
For a type like:
   data Product a b .. = MkProduct { getA :: a, getB :: b, .. }
This generates the following instance:

instance (AutoReg a, AutoReg b, ..) => AutoReg (Product a b ..) where
  autoReg' autoDeep clk rst en initVal input =
    MkProduct <$> sig0 <*> sig1 ...
    where
      field0 = (\(MkProduct x _ ...) -> x) <$> input
      field1 = (\(MkProduct _ x ...) -> x) <$> input
      ...
      MkProduct initVal0 initVal1 ... = initVal
      sig0 = suffixName @"getA" reg clk rst en initVal0 field0
      sig1 = suffixName @"getB" reg clk rst en initVal1 field1
      ...
      reg :: forall dom t. (AutoReg t, KnownDomain dom) -> Clock dom -> ...
      reg = if autoDeep then autoReg else register
-}
deriveAutoRegProduct :: DatatypeInfo -> ConstructorInfo -> DecsQ
deriveAutoRegProduct tyInfo conInfo = go (constructorName conInfo) fieldInfos
  where
    tyNm = datatypeName tyInfo
    tyVarBndrs = datatypeVars tyInfo

    fieldInfos = zip fieldNames (constructorFields conInfo)
      where
        fieldNames = case constructorVariant conInfo of
          RecordConstructor nms -> map Just nms
          _ -> repeat Nothing

    go :: Name -> [(Maybe Name,Type)] -> Q [Dec]
    go dcNm fields = do
      let fieldNames = map fst fields
      args <- mapM newName ["autoDeep", "clk","rst","en","initVal","input"]
      reg <- newName "reg"
      let [autoDeepE,clkE,rstE,enE,initValE,inputE] = map varE args
          regE = varE reg
          regP = varP reg
      let
        tyVars = map (VarT . bndrName) tyVarBndrs
        ty = conAppsT tyNm tyVars
        argsP = map varP args
      parts <- generateNames "field" fields
      let
        field :: Name -> Int -> DecQ
        field nm nr = valD (varP nm) (normalB [|$fieldSel <$> $inputE|]) []
          where
            fieldSel = do
              xNm <- newName "x"
              let fieldP = [ if nr == n then varP xNm else wildP
                           | (n,_) <- zip [0..] fields]
              lamE [conP dcNm fieldP] (varE xNm)   -- "\(Dc _ _ .. x _ ..) -> x"
      fieldDecls <- sequence $ zipWith field parts [0..]
      sigs <- generateNames "sig" fields
      initVals <- generateNames "initVal" fields
      let initPat = conP dcNm (map varP initVals)
      initDecl <- valD initPat (normalB initValE) []

      let
        genAutoRegDecl :: PatQ -> ExpQ -> ExpQ -> Maybe Name -> DecsQ
        genAutoRegDecl s v i nameM =
            [d| $s = $nameMe $regE $clkE $rstE $enE $i $v |]
            where
              nameMe = case nameM of
                Nothing -> [| id |]
                Just nm -> let nmSym = litT $ strTyLit (nameBase nm)
                           in [| suffixName @($nmSym) |]
      partDecls <- concat <$> (sequence $ zipWith4 genAutoRegDecl
                                                   (varP <$> sigs)
                                                   (varE <$> parts)
                                                   (varE <$> initVals)
                                                   (fieldNames)
                              )
      regDecl <- [d| $(regP) = (if $autoDeepE then autoReg else register) |]

      -- Type signature is needed here because MonoLocalBinds is on,
      -- and we want keep reg polymorphic in `t`.
      regTyDecl <- sigD reg [t| forall dom t . (KnownDomain dom,AutoReg t)
                                => Clock dom -> Reset dom -> Enable dom
                                -> t
                                -> Signal dom t -> Signal dom t
                            |]
      let
          decls :: [DecQ]
          decls = map pure (initDecl : fieldDecls ++ partDecls ++ regTyDecl : regDecl)
          tyConE = conE dcNm
          body = case map varE sigs of
                  (sig0:rest) -> foldl (\acc sigN -> [| $acc <*> $sigN |])  [| $tyConE <$> $sig0 |] rest
                  [] -> fail "Can't deriveAutoReg for types with constructors without fields"

      autoRegDec <- funD 'autoReg' [clause argsP (normalB body) decls]
      ctx <- calculateRequiredContext conInfo
      return [InstanceD Nothing ctx (AppT (ConT ''AutoReg) ty) [autoRegDec]]

-- Calculate the required constraint to call autoReg on all the fields of a given constructor
calculateRequiredContext :: ConstructorInfo -> Q Cxt
calculateRequiredContext conInfo = do
  let fieldTys = constructorFields conInfo
  wantedInstances <- mapM (\ty -> constraintsWantedFor ''AutoReg [ty]) $ nub fieldTys
  return $ nub $ concat wantedInstances

constraintsWantedFor :: Name -> [Type] -> Q Cxt
constraintsWantedFor clsNm tys
  | show clsNm == "GHC.TypeNats.KnownNat" = do
  -- KnownNat is special, you can't just lookup instances with reifyInstances.
  -- So we just pass KnownNat constraints.
  -- This will most likely require UndecidableInstances.
    return [conAppsT clsNm tys]
constraintsWantedFor clsNm [ty] = case ty of
  VarT _ -> return [AppT (ConT clsNm) ty]
  ConT _ -> return []
  _ -> do
    insts <- reifyInstances clsNm [ty]
    case insts of
      []      -> fail $ "Missing instance AutoReg (" ++ pprint ty ++ ")"
      (_:_:_) -> fail $ "There are multiple AutoReg instances for " ++ pprint ty ++ ":\n" ++ pprint insts
      [InstanceD _ cxtInst (AppT autoRegCls instTy) _]
        | autoRegCls == ConT clsNm -> do
          let substs = findTyVarSubsts instTy ty
              cxt2 = map (applyTyVarSubsts substs) cxtInst
              okCxt = filter isOk cxt2
              recurseCxt = filter needRecurse cxt2
          recursed <- mapM recurse recurseCxt
          return (okCxt ++ concat recursed)
      _ -> fail $ "Got unexpected instance: " ++ pprint insts
constraintsWantedFor clsNm tys =
  return [conAppsT clsNm tys] -- see [NOTE: MultiParamTypeClasses]

isOk :: Type -> Bool
isOk (unfoldType -> (_cls,tys)) =
  case tys of
    [VarT _] -> True
    [_] -> False
    _ -> True -- see [NOTE: MultiParamTypeClasses]
needRecurse :: Type -> Bool
needRecurse (unfoldType -> (cls,tys)) =
  case tys of
    [VarT _] -> False
    [ConT _] -> False  -- we can just drop constraints like: "AutoReg Bool => ..."
    [AppT _ _] -> True
    [_] -> error ("Error while deriveAutoReg: don't know how to handle: " ++ pprint cls ++ " (" ++ pprint tys ++ ")")
    _ -> False -- see [NOTE: MultiParamTypeClasses]

recurse :: Type -> Q Cxt
recurse (unfoldType -> (ConT cls,tys)) = constraintsWantedFor cls tys
recurse ty = fail ("Expected a constraint applied to some arguments but got " ++ pprint ty)

-- [NOTE: MultiParamTypeClasses]
-- The constraint calculation code doesn't handle MultiParamTypeClasses "properly".
-- But it will try to pass them on, so the resulting instance should still
-- compile with UndecidableInstances enabled.


-- | Find tyVar substitutions between a general type
-- and a second possibly less general type.
-- Example:
-- @
-- findTyVarSubsts "Either a b" "Either c [Bool]"
--   == "[(a,c), (b,[Bool])]"
-- @
findTyVarSubsts :: Type -> Type -> [(Name,Type)]
findTyVarSubsts = go
  where
  go ty1 ty2 = case (ty1,ty2) of
    (VarT nm, t) -> [(nm,t)]
    (ConT _, ConT _) -> []
    (AppT x1 y1, AppT x2 y2)           -> go x1 x2 ++ go y1 y2
    (AppKindT t1 k1, AppKindT t2 k2)   -> go t1 t2 ++ go k1 k2
    (SigT t1 k1, SigT t2 k2)           -> go t1 t2 ++ go k1 k2
    (InfixT x1 _ y1, InfixT x2 _ y2)   -> go x1 x2 ++ go y1 y2
    (UInfixT x1 _ y1, UInfixT x2 _ y2) -> go x1 x2 ++ go y1 y2
    (ParensT x1, ParensT x2)            -> go x1 x2
    (ImplicitParamT _ x1, ImplicitParamT _ x2) -> go x1 x2
    -- TODO ForAllT?
    _ -> []

applyTyVarSubsts :: [(Name,Type)] -> Type -> Type
applyTyVarSubsts substs ty = go ty
  where
    go ty' = case ty' of
      VarT n -> case lookup n substs of
                  Nothing -> ty'
                  Just m  -> m
      ConT _ -> ty'
      AppT ty1 ty2 -> AppT (go ty1) (go ty2)
      _ -> error $ "TODO applyTyVarSubsts: " ++ show ty'


-- | Generate a list of fresh Name's:
-- prefix0_.., prefix1_.., prefix2_.., ..
generateNames :: String -> [a] -> Q [Name]
generateNames prefix xs = sequence (zipWith (\n _ -> newName $ prefix ++ show @Int n) [0..] xs)

deriveAutoRegTuples :: [Int] -> DecsQ
deriveAutoRegTuples xs = concat <$> mapM deriveAutoRegTuple xs

deriveAutoRegTuple :: Int -> DecsQ
deriveAutoRegTuple n
  | n < 2 = fail $ "deriveAutoRegTuple doesn't work for " ++ show n ++ "-tuples"
  | otherwise = deriveAutoReg tupN
  where
    tupN = mkName $ "(" ++ replicate (n-1) ',' ++ ")"
