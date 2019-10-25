{-|
Copyright  :  (C) 2019, Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Verification
-}

{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}

module Clash.Verification.PrettyPrinters
  ( pprProperty
  , pprPslAssertion
  ) where

import           Clash.Annotations.Primitive      (HDL(..))
import           Clash.Signal.Internal            (ActiveEdge, ActiveEdge(..))
import           Clash.Verification.Types
import           Data.Maybe                       (fromMaybe)
import qualified Data.Text                        as Text
import           Data.Text                        (Text)
import           TextShow                         (showt)

data Symbol
  = TImpliesOverlapping
  | TImplies
  | Implies
  | BiImplies
  | Not
  | And
  | Or
  | To
  -- + [] ?
  | Assign
  | Is

pprEdge :: HDL -> ActiveEdge -> Text -> Text
pprEdge SystemVerilog activeEdge clkId = pprEdge Verilog activeEdge clkId
pprEdge Verilog Rising clkId = "posedge " <> clkId
pprEdge Verilog Falling clkId = "negedge " <> clkId
pprEdge VHDL Rising clkId = "rising_edge(" <> clkId <> ")"
pprEdge VHDL Falling clkId = "falling_edge(" <> clkId <> ")"

-- | Taken from IEEE Std 1850-2010a, Annex B.1, p149
symbol :: HDL -> Symbol -> Text
symbol SystemVerilog = symbol Verilog
symbol Verilog = \case
  TImpliesOverlapping -> "|->"
  TImplies  -> "|=>"
  Implies   -> "->"
  BiImplies -> "<->"
  Not       -> "!"
  And       -> "&&"
  Or        -> "||"
  To        -> ":"
  Assign    -> "<="
  Is        -> "="

symbol VHDL = \case
  TImpliesOverlapping -> "|->"
  TImplies  -> "|=>"
  Implies   -> "->"
  BiImplies -> "<->"
  Not       -> "not "
  And       -> "and"
  Or        -> "or"
  To        -> " to "
  Assign    -> "<="
  Is        -> "is"

-- | Pretty print CvProperty. Doesn't print valid HDL, but can be used for
-- debugging purposes.
pprProperty :: CvProperty dom -> Text
pprProperty prop = pprPslProperty' VHDL (fromMaybe "__autogen__" . fst <$> prop)

------------------------------------------
--                 UTIL                 --
------------------------------------------
-- | Collapse constructs such as `next (next a)` down to `next[2] a`
squashAfter :: CvExpression' a -> [CvExpression' a]
squashAfter (CvAfter e1 e2) = e1s ++ e2s
 where
  e1s = case squashAfter e1 of {[] -> [e1]; es -> es}
  e2s = case squashAfter e2 of {[] -> [e2]; es -> es}
squashAfter _ = []

parensIf :: Bool -> Text -> Text
parensIf True s = "(" <> s <> ")"
parensIf False s = s

binOp
  :: HDL
  -> Bool
  -> Symbol
  -> CvExpression' Text
  -> CvExpression' Text
  -> Text
binOp hdl parens op e1 e2 =
  parensIf parens (e1' <> symbol hdl op <> e2')
 where
  e1' = pprPslExpr' hdl True e1
  e2' = pprPslExpr' hdl True e2

------------------------------------------
--              PSL VHDL                --
------------------------------------------
pprPslAssertion
  :: HDL
  -- ^ HDL to generate PSL expression for
  -> Text
  -- ^ Property name
  -> Text
  -- ^ Clock name
  -> ActiveEdge
  -- ^ Edge property should be sensitive to
  -> CvAssertion' Text
  -- ^ Assertion / Cover statement
  -> Text
pprPslAssertion hdl propName clkId edge assertion =
  "psl property " <> propName <> " " <> symbol hdl Is <> "\n" <>
  "(" <> prop <> ") @(" <> pprEdge hdl edge clkId <> ")" <>
  ";\n" <> "psl " <> coverOrAssert <> " " <>
  propName <> ";"
 where
  (coverOrAssert, prop) =
    case assertion of
      CvCover e -> ("cover", pprPslProperty' hdl e)
      CvAssert e -> ("assert", pprPslProperty' hdl e)

pprPslProperty' :: HDL -> CvProperty' Text -> Text
pprPslProperty' hdl = \case
  (CvAlways e) -> "always " <> pprPslExpr' hdl False e
  (CvNever e) -> "never" <> pprPslExpr' hdl False e

pprPslExpr' :: HDL -> Bool -> CvExpression' Text -> Text
pprPslExpr' hdl parens e =
  case e of
    (CvPure p) -> p
    (CvLit False) -> "false"
    (CvLit True) -> "true"

    (CvAnd e1 e2) -> binOp1 And e1 e2
    (CvOr e1 e2) -> binOp1 Or e1 e2
    (CvImplies e1 e2) -> binOp1 Implies e1 e2

    (CvNext 0 e1) -> pprPslExpr' hdl parens e1
    (CvNext 1 e1) -> "next " <> pprPslExpr' hdl True e1
    (CvNext n e1) -> "next[" <> showt n <> "] " <> pprPslExpr' hdl False e1

    (CvAfter _ _) -> "{" <> afters1 <> "}"
     where
      afters0 = map (pprPslExpr' hdl False) (squashAfter e)
      afters1 = foldl1 (\e1 e2 -> e1 <> "; " <> e2) afters0

    (CvTemporalImplies 0 e1 e2) -> binOp1 TImpliesOverlapping e1 e2
    (CvTemporalImplies 1 e1 e2) -> binOp1 TImplies e1 e2
    (CvTemporalImplies n e1 e2) -> binOp1 TImplies e1 (CvNext n e2)
 where
  binOp1 = binOp hdl parens
