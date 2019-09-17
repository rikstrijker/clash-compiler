{-|
Copyright  :  (C) 2019, Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE ViewPatterns #-}

module Clash.XException.CodeGen (mkNFDataXTupleInstances) where

import           Language.Haskell.TH.Syntax
import           Clash.CPP                    (maxTupleSize)


mkTup :: [Type] -> Type
mkTup names@(length -> n) =
  foldl AppT (TupleT n) names

-- | Creates an instance of the form:
--
--  instance (NFDataX a0, NFDataX a1) => NFDataX (a0, a1)
--
-- With /n/ number of variables.
mkNFDataXTupleInstance :: Name -> Int -> Dec
mkNFDataXTupleInstance nfdataxName n =
  InstanceD Nothing constraints instanceTyp []
 where
  constraints = map (AppT (ConT nfdataxName)) vars
  instanceTyp = ConT nfdataxName `AppT` mkTup vars
  vars = map (VarT . mkName . ("a"++) . show) [0..n-1]


mkNFDataXTupleInstances :: Name -> Q [Dec]
mkNFDataXTupleInstances nfdataxName =
  let toTupSize = min 15 maxTupleSize in
  pure (map (mkNFDataXTupleInstance nfdataxName) [2..toTupSize])
