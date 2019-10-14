{-

Generate blackbox functions with no meaningful simulation

makeBlackboxScaffold "xilinxDiffClock" "IBUFDS_GTE2" "_transceiver" []
  [ ClkOut "O"
  , ClkIn "I"
  , ClkIn "IB"
  ]

  ===>
    data XilinxDiffClockBlackboxIn (dom_a1jeS :: Domain)
      = XilinxDiffClockBlackboxIn {clkI :: (Clock dom_a1jeS),
                                   clkIB :: (Clock dom_a1jeS)}
      deriving Generic
    data XilinxDiffClockBlackboxOut (dom_a1jeS :: Domain)
      = XilinxDiffClockBlackboxOut {clkO :: (Clock dom_a1jeS)}
      deriving Generic
    xilinxDiffClock ::
      forall (dom_a1jeS :: Domain).
      KnownDomain dom_a1jeS =>
      Clock dom_a1jeS
      -> Clock dom_a1jeS
         -> Blackboxes.XilinxDiffClockBlackboxOut dom_a1jeS
    xilinxDiffClock !_ !_
      = Blackboxes.XilinxDiffClockBlackboxOut clockGen
    xilinxDiffClockTF
      = ((Clash.Netlist.Types.TemplateFunction
        ...

-}

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveLift            #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ViewPatterns          #-}

module Clash.Primitives.Scaffold
  ( makeBlackboxScaffold
  , Port (..)
  , Parameter(..)
  )
  where

import           Prelude

import           Language.Haskell.TH
import           GHC.Generics (Generic)
import           Data.Char                      ( toUpper, toLower )
import qualified Data.Text                     as T
import           Data.List                      ( sortOn )
import           Data.Semigroup.Monad           ( getMon )
import           Control.Monad.State            ( State )

import qualified Data.String.Interpolate       as I

import           Clash.Netlist.Id               ( IdType(Basic) )
import           Clash.Netlist.Types     hiding ( PortDirection(..) )
import           Clash.Netlist.Types            ( PortDirection )
import qualified Clash.Netlist.Types           as Netlist
import           Data.Text.Prettyprint.Doc.Extra
                                                ( Doc )
import           Clash.Annotations.Primitive
import           Clash.Backend                  ( Backend
                                                , mkUniqueIdentifier
                                                , blockDecl
                                                )

import qualified Clash.Prelude                 as C
                                         hiding ( Text )
import           Clash.Prelude                  ( Lift
                                                , KnownNat
                                                , natVal
                                                , SNat(..)
                                                , def
                                                )


type Width = Integer

data Port
  = In String Width
  | Out String Width
  | ClkIn String
  | ClkOut String

-- *

type IsClock = Bool
type HWParameter = (Expr,HWType,Expr)

data Parameter
deriving instance Lift (Parameter)
mkParam :: a -> HWParameter
mkParam _ = undefined

data ScaffoldPort
  = ScaffoldPort
  { direction :: PortDirection
  , width :: Width
  , name :: String
  , isClock :: IsClock
  , ty :: Type
  , domain :: Name
  }
  deriving (Show, Lift)

mkTmplTy :: IsClock -> Width -> Name -> Type
mkTmplTy True _ dom  = AppT (ConT ''C.Clock) (VarT dom)
mkTmplTy False w dom =
  AppT
    (AppT (ConT ''C.Signal) (VarT dom))
    (AppT (ConT ''C.BitVector) (LitT $ NumTyLit w))

scaffoldPort :: Name -> Port -> ScaffoldPort
scaffoldPort d (In n w) = ScaffoldPort Netlist.In w n False (mkTmplTy False w d) d
scaffoldPort d (Out n w) = ScaffoldPort Netlist.Out w n False (mkTmplTy False w d) d
scaffoldPort d (ClkIn n) = ScaffoldPort Netlist.In 1 n True (mkTmplTy True 1 d) d
scaffoldPort d (ClkOut n) = ScaffoldPort Netlist.Out 1 n True (mkTmplTy True 1 d) d

scaffoldDomain :: [Port] -> Q (Name, [ScaffoldPort])
scaffoldDomain ps = do
  d <- newName "domain"
  return $ (d, scaffoldPort d <$> ps)

mkHwTy :: ScaffoldPort -> HWType
mkHwTy (ScaffoldPort _ _ _ True _ _) = Clock (T.pack "clk")
mkHwTy (ScaffoldPort _ w _ False _ _) = BitVector (fromInteger w)

mkArg
  :: ScaffoldPort
  -> (Expr,HWType)
  -> (Expr, PortDirection, HWType, Expr)
mkArg (ScaffoldPort Netlist.In _ n _ _ _) (e,t)
  = (Identifier (T.pack n) Nothing, Netlist.In,  t, e)
mkArg (ScaffoldPort Netlist.Out _ n _ _ _) (e,t)
  = ( Identifier (T.pack n) Nothing
    , Netlist.Out, t, e)

scaffoldTemplate
  :: Backend s
  => String -> [Parameter] -> [Name] -> [ScaffoldPort] -> [ScaffoldPort]
  -> BlackBoxContext
  -> State s Doc
scaffoldTemplate primitiveName parameters domains i o bbCtx = do
  wires <- mapM (mkId . T.pack . name) o
  inst  <- mkId (T.pack $ primitiveName<>"_inst")
  blk   <- mkId (T.pack $ primitiveName<>"_blk")

  let instArgo = zipWith3 (\a b c -> mkArg a (b,c)) o (flip Identifier Nothing <$> wires) wiresTy

  getMon $ blockDecl blk $
     [ NetDecl Nothing o' t | (o',t) <- zip wires wiresTy] <>
     [ InstDecl Comp Nothing (T.pack primitiveName) inst
         paramsTy
         (instArgi <> instArgo)
     , result wires (bbResult bbCtx)
     ]
 where
  args = drop (length domains) (bbInputs bbCtx)
  mkId = mkUniqueIdentifier Basic
  wiresTy = fmap mkHwTy o
  paramsTy = mkParam <$> parameters

  dropTrip (a,b,_) = (a,b)
  instArgi = zipWith mkArg i (fmap dropTrip args)

  result wires (Identifier r Nothing, resTy@(Product _ _ _)) =
                Assignment r (DataCon resTy (DC (resTy,0)) $ [ Identifier w Nothing | w <- wires ])
  result wires (Identifier r Nothing, _) | [wire] <- wires  =
                Assignment r (Identifier wire Nothing)
  result _ _ = error "scaffoldTemplate: unexpected result type"

scaffoldTF
  :: [Int] -> String -> [Parameter] -> [Name] -> [ScaffoldPort] -> [ScaffoldPort]
  -> TemplateFunction
scaffoldTF used primitiveName parameters domains i o =
  TemplateFunction used (const True) (scaffoldTemplate primitiveName parameters domains i o)

scaffoldAnnotation :: HDL -> Name -> Name -> Q Exp
scaffoldAnnotation hdl n ntf =
  [|InlinePrimitive hdl j|]
 where
  j = [I.i| [{ "BlackBox" :
              { "name" : "#{n}"
              , "kind": "Declaration"
              , "format": "Haskell"
              , "templateFunction" : "#{ntf}"
              }
          }]
      |]

collectDomains :: [(Name, [ScaffoldPort])] -> ([Name], [ScaffoldPort])
collectDomains = foldl (\(ns, pss) (n,ps) -> (n:ns,ps<>pss)) ([],[])

makeDatatypes
  :: String
  -> (String -> String) -- ^ record renaming
  -> ([TyVarBndr],[Name])
  -> [ScaffoldPort]
  -> [ScaffoldPort]
  -> [Dec]
makeDatatypes fname f (kinds,domains) i o =
  [ build "I" (mkRec <$> i) [''Generic] -- TODO: custom constructors taking clocks
  , build "O" (mkRec <$> o) [''Generic]

  , SigD (fn fname)
    $ ForallT kinds constraints
    $ foldr (AppT . AppT ArrowT) retTy (ty <$> iclks)
  , FunD (fn fname) [Clause (VarP <$> argNames) (NormalB (
      foldl AppE
        (foldl AppE (ConE (mkName $ fname <>"I")) (VarE <$> argNames))
        (replicate (length i - length iclks) pd)
      )) []]
  ]
 where
  b = Bang NoSourceUnpackedness NoSourceStrictness
  -- classes = [''Generic, ''Default, ''Show, ''ShowX, ''NFDataX] -- TODO: ?
  drv = (:[]) . DerivClause Nothing . fmap ConT
#if MIN_VERSION_template_haskell(2,11,0)
  build suffix fields derive = DataD [] (mkName $ fname<>suffix) kinds Nothing [(RecC (mkName $ fname<>suffix) fields)] (drv derive)
#else
  build suffix fields derive = DataD [] (mkName $ fname<>suffix) kinds [(RecC (mkName $ fname<>suffix) fields)] (drv derive)
#endif
  fn (n:ame) = mkName $ toLower n : (ame <> "I")
  fn _ = error "Empty name!"
  constructor = (mkName $ fname <>"I")
  constraints = AppT (ConT ''C.KnownDomain) . VarT <$> domains
  iclks = filter isClock i
  argNames = zipWith (const (mkName . (<>) "clkArg" . show)) iclks [0::Int ..]
  applyDomains = flip (foldl AppT) (fmap VarT domains)
  retTy = applyDomains $ ConT $ constructor
  pd = (VarE 'pure) `AppE` (VarE 'def)
  mkRec (ScaffoldPort _ _ n _ t _) = (mkName (f n), b, t)

makeTemplate
  :: String
  -> String
  -> [Parameter]
  -> [Name]
  -> [ScaffoldPort]
  -> [ScaffoldPort]
  -> DecsQ
makeTemplate fname primitive parameters domains i o = do
  currLoc <- location
  let currMod = loc_module currLoc <> "."
  let tName = mkName $ fname <> "TF"
  let qfName = mkName $ currMod <> fname <> "#"
  let qtName = mkName $ currMod <> fname <> "TF"

  blackboxAnn  <- PragmaD . AnnP (valueAnnotation qfName)
                  <$> scaffoldAnnotation Verilog qfName qtName
  blackboxExpr <- [| scaffoldTF [length domains .. length domains + length i - 1] primitive parameters domains i o |]

  return
    [ SigD tName (ConT ''TemplateFunction)
    , FunD tName [Clause [] (NormalB blackboxExpr) []]
    , blackboxAnn
    ]

makeWrapper
  :: String
  -> String
  -> (String -> String)
  -> ([TyVarBndr], [Name])
  -> [ScaffoldPort]
  -> [ScaffoldPort]
  -> [Dec]
makeWrapper fname dataname f (kinds, domains) i o =
  [ SigD name#
    $ ForallT kinds constraints
    $ foldr1 (AppT . AppT ArrowT) (fmap ty i ++ [retTy])
  , FunD name# [Clause bangs (NormalB ret) []]
  , PragmaD (InlineP name# NoInline FunLike AllPhases)
  , SigD name'
    $ ForallT kinds constraints
    $ AppT ArrowT argTy `AppT` retTy
  , FunD name' [Clause [TupP [VarP arg]] (NormalB (
      foldl AppE (VarE name#) $ fmap (flip AppE (VarE arg) . VarE . mkName . f . name) i
      )) []]
  , PragmaD (InlineP name' NoInline FunLike AllPhases)
  ]
 where
  arg = (mkName "_arg")
  name# = mkName (fname <> "#")
  name' = mkName fname
  pd = (VarE 'pure) `AppE` (VarE 'def)
  bangs = replicate (length i) (BangP WildP)
  ret= foldl AppE (ConE $ mkName $ dataname <> "O")
          $ gen . isClock <$> o
  gen True = VarE 'C.clockGen
  gen False = pd
  constraints = AppT (ConT ''C.KnownDomain) . VarT <$> domains
  applyDomains = flip (foldl AppT) (fmap VarT domains)
  retTy = applyDomains $ ConT $ mkName $ dataname <> "O"
  argTy = applyDomains $ ConT $ mkName $ dataname <> "I"

makeBlackboxScaffold
  :: String -- ^ generated haskell function name
  -> String -- ^ hdl primitive name
  -> (String -> String) -- ^ Record namer
  -> [Parameter]
  -> [[Port]]
  -> DecsQ
makeBlackboxScaffold haskname primitive portToRecord parameters (fmap scaffoldDomain -> qdomains) = do
  let dataname = (\(n:ame) -> toUpper n : ame) haskname

  (domains, ports) <- collectDomains <$> sequence qdomains

  let kinds = flip KindedTV (ConT ''C.Domain) <$> domains

  let i = sortOn (not . isClock) $ filterDir (Netlist.In) ports
  let o = sortOn (not . isClock) $ filterDir (Netlist.Out) ports

  mappend (mconcat
   [ makeDatatypes dataname portToRecord (kinds,domains) i o
   , makeWrapper haskname dataname portToRecord (kinds, domains) i o
   ]) <$> makeTemplate haskname primitive parameters domains i o
 where
  filterDir dir = filter ((dir==) . direction)
