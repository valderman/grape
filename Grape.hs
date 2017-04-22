{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
-- | Grape: Generic Reification of Algebraics and Patterns for EDSLs
module Grape
  ( Algebraic (..), Exp, Stm, Pat, Var, Bind
  , module Control.Monad
  , true, false, undef, (.==), (!=), (.>), (.<), (.>=), (.<=), not_
  , printS, printN, scanN, if_, newRef, getRef
  , inj, wc, Grape.var, val, (~>), with, match', matchDef, new
  , Grape.compile, Grape.compileAndRun
  ) where
import Pat hiding (Exp, if_)
import Exp
import Stm hiding (Bind)
import Comp
import Control.Monad
import Control.Shell
import Data.Proxy -- for wc

if_ :: Exp Bool -> Stm (Exp a) -> Stm (Exp a) -> Stm (Exp a)
if_ = If

compile :: FilePath -> Stm () -> IO ()
compile f p = shell_ $ Comp.compile f p

compileAndRun :: Stm () -> IO ()
compileAndRun = shell_ . Comp.compileAndRun

printS :: String -> Stm ()
printS = Print

printN :: Exp Int -> Stm ()
printN = PrintN

scanN :: Stm (Exp Int)
scanN = Scan

newRef :: Exp a -> Stm (Var a)
newRef = NewRef

getRef :: Var a -> Stm (Exp a)
getRef = Get

setRef :: Var a -> Exp a -> Stm ()
setRef = Set

newtype Bind a = Bind {val :: Exp a}

var :: (Typeable a, Algebraic Stm a) => Bind a -> a
var (Bind (Var v)) = Stm.var v

with :: (Bind a -> Stm b) -> Stm b
with f = do
  x <- newRef undef
  f (Bind $ Var x)

instance Algebraic Stm (Exp Bool) where encode = encodePrim . B2I

instance Algebraic Stm Int where encode = encodePrim . Const
instance Algebraic Stm Bool where encode = encode . Bool

-- | An unnamed wildcard.
wc :: Algebraic Stm a => a
wc = wcFor (Proxy :: Proxy Stm)
