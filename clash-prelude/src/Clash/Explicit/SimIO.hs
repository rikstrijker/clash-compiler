{-|
  Copyright   :  (C) 2019, Google Inc
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE BangPatterns, MagicHash, TypeOperators, ScopedTypeVariables, FlexibleContexts #-}

module Clash.Explicit.SimIO
  ( SimIO
  , mealyIO
  , display
  , finish
  -- * Mutable values
  , Reg
  , reg
  , readReg
  , writeReg
  -- * Files
  , File
  , openFile
  , fgetc
  , feof
  )
where

import Data.Coerce
import Data.IORef
import qualified System.IO as IO
import System.IO.Unsafe

import Clash.Signal.Internal
import Clash.XException (seqX)

newtype SimIO a = SimIO {unSimIO :: IO a}

instance Functor SimIO where
  fmap = fmapSimIO#

fmapSimIO# :: (a -> b) -> SimIO a -> SimIO b
fmapSimIO# f (SimIO m) = SimIO (fmap f m)
{-# NOINLINE fmapSimIO# #-}

instance Applicative SimIO where
  pure  = pureSimIO#
  (<*>) = apSimIO#

pureSimIO# :: a -> SimIO a
pureSimIO# a = SimIO (pure a)
{-# NOINLINE pureSimIO# #-}

apSimIO# :: SimIO (a -> b) -> SimIO a -> SimIO b
apSimIO# (SimIO f) (SimIO m) = SimIO (f <*> m)
{-# NOINLINE apSimIO# #-}

instance Monad SimIO where
  return = pureSimIO#
  (>>=)  = bindSimIO#

bindSimIO# :: SimIO a -> (a -> SimIO b) -> SimIO b
bindSimIO# (SimIO m) k = SimIO (m >>= (\x -> x `seqX` coerce k x))
{-# NOINLINE bindSimIO# #-}

display
  :: String
  -> SimIO ()
display s = SimIO (putStrLn s)
{-# NOINLINE display #-}

finish
  :: Integer
  -> SimIO a
finish i = return (error (show i))
{-# NOINLINE finish #-}

newtype Reg a = Reg (IORef a)

reg :: a -> SimIO (Reg a)
reg a = SimIO (Reg <$> newIORef a)
{-# NOINLINE reg #-}

readReg :: Reg a -> SimIO a
readReg (Reg a) = SimIO (readIORef a)
{-# NOINLINE readReg #-}

writeReg :: Reg a -> a -> SimIO ()
writeReg (Reg r) a = SimIO (writeIORef r a)
{-# NOINLINE writeReg #-}

newtype File = File IO.Handle

openFile :: FilePath -> SimIO File
openFile fp = coerce (IO.openFile fp IO.ReadWriteMode)
{-# NOINLINE openFile #-}

fgetc :: File -> SimIO Char
fgetc (File fp) = SimIO (IO.hGetChar fp)
{-# NOINLINE fgetc #-}

feof :: File -> SimIO Bool
feof (File fp) = SimIO (IO.hIsEOF fp)
{-# NOINLINE feof #-}

mealyIO
  :: KnownDomain dom
  => Clock dom
  -> (s -> dutOut -> SimIO dutIn)
  -> SimIO s
  -> Signal dom dutOut
  -> Signal dom dutIn
mealyIO !_ f (SimIO i) inp = unsafePerformIO (i >>= go inp)
 where
  go q@(~(k :- ks)) s =
    (:-) <$> unSimIO (f s k) <*> unsafeInterleaveIO ((q `seq` go ks s))
{-# NOINLINE mealyIO #-}
