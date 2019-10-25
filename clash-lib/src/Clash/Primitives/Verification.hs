{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}

module Clash.Primitives.Verification (assertBBF) where

import Data.Either


import qualified Control.Lens                    as Lens
import           Control.Monad.State             (State)
import           Data.Text.Prettyprint.Doc.Extra (Doc)
import qualified Data.Text                       as Text
import           Data.Semigroup.Monad            (getMon)
import           GHC.Stack                       (HasCallStack)

import qualified Clash.Backend
import           Clash.Backend                   (Backend, blockDecl, hdlKind)
import           Clash.Core.Term                 (Term(Var))
import           Clash.Core.Util                 (termType)
import           Clash.Util                      (indexNote)
import           Clash.Util.TermLiteral
  (uncheckedTermToData, termToDataError)
import           Clash.Netlist                   (mkExpr)
import           Clash.Netlist.BlackBox.Util     (stripVoid)
import qualified Clash.Netlist.Util
import           Clash.Netlist.Util              (id2identifier)
import           Clash.Netlist.Id                (IdType(Basic))
import           Clash.Netlist.Types
  (BlackBox(BBFunction), TemplateFunction(..), BlackBoxContext, Identifier,
   NetlistMonad, Declaration(Assignment, NetDecl', CommentDecl),
   HWType(Bool, KnownDomain), WireOrReg(Wire), tcCache, bbInputs, bbResult)
import           Clash.Netlist.BlackBox.Types
  (BlackBoxFunction, BlackBoxMeta(..), TemplateKind(TDecl),
   emptyBlackBoxMeta)

import           Clash.Verification.Types
import           Clash.Verification.PrettyPrinters

import Debug.Trace

assertBBF :: BlackBoxFunction
assertBBF _isD _primName args _ty =
  case cvProperty of
    Left err -> pure (Left err)
    Right cvExpr0 -> do
      cvExpr1 <- mapM (uncurry bindMaybe) cvExpr0
      let decls = concatMap snd cvExpr1
          cvExpr2 = fmap fst cvExpr1
      pure (Right (meta, bb (assertTF decls clkId propName renderAs cvExpr2)))
 where
  (Var (id2identifier -> clkId)) = indexNote "clk" (lefts args) 1
  propName = uncheckedTermToData (indexNote "propName" (lefts args) 2)
  renderAs = uncheckedTermToData (indexNote "renderAs" (lefts args) 3)
  cvProperty = CvAssert <$> termToDataError (indexNote "propArg" (lefts args) 4)

  bb = BBFunction "Clash.Primitives.Verification.assertTF" 0
  meta = emptyBlackBoxMeta {bbKind=TDecl}
  mkId = Clash.Netlist.Util.mkUniqueIdentifier Basic . Text.pack

  bindMaybe
    :: Maybe String
    -- ^ Hint for new identifier
    -> Term
    -- ^ Term to bind. Does not bind if it's already a reference to a signal
    -> NetlistMonad (Identifier, [Declaration])
    -- ^ ([new] reference to signal, [declarations need to get it in scope])
  bindMaybe _ (Var vId) = pure (id2identifier vId, [])
  bindMaybe Nothing t = bindMaybe (Just "s") t
  bindMaybe (Just nm) t = do
    tcm <- Lens.use tcCache
    newId <- mkId nm
    (expr, decls) <- mkExpr False (Left newId) (termType tcm t) t
    pure
      ( newId
      , decls ++ [sigDecl Bool newId, Assignment newId expr] )

  -- Simple wire without comment
  sigDecl :: HWType -> Identifier -> Declaration
  sigDecl typ nm = NetDecl' Nothing Wire nm (Right typ) Nothing

assertTF
  :: [Declaration]
  -> Identifier
  -> Text.Text
  -> CvRenderAs
  -> CvAssertion' Identifier
  -> TemplateFunction
assertTF decls clkId propName renderAs prop =
  TemplateFunction [] (const True) (assertTF' decls clkId propName renderAs prop)

assertTF'
  :: forall s
   . (HasCallStack, Backend s)
  => [Declaration]
  -- ^ Extra decls needed
  -> Identifier
  -- ^ Clock
  -> Text.Text
  -- ^ Prop name
  -> CvRenderAs
  -> CvAssertion' Identifier
  -> BlackBoxContext
  -> State s Doc
assertTF' decls clkId propName renderAs prop bbCtx = do
  blockName <- mkId (propName <> "_block")
  traceShow (bbResult bbCtx) $ getMon $ blockDecl blockName $ (++) decls $
    [ CommentDecl renderedPslProperty
    ]

 where
  hdl = hdlKind (undefined :: s)

  edge =
    case bbInputs bbCtx !! 0 of
      (_, stripVoid -> KnownDomain _nm _period e _rst _init _polarity, _) -> e
      _ -> error $ "Unexpected first argument: " ++ show (bbInputs bbCtx !! 0)

  renderedPslProperty =
    case renderAs of
      PSL -> pprPslAssertion hdl propName clkId edge prop
      SVA ->
        error "SVA NYI"

  mkId :: Text.Text -> State s Identifier
  mkId = Clash.Backend.mkUniqueIdentifier Basic
