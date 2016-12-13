{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
-- | Grape: Generic Reification of ADTs and Patterns for EDSLs
module Grape
  ( ADT (..), Exp, Stm, Pat, Var, Bind
  , module Control.Monad
  , true, false, undef, (.==), (!=), (.>), (.<), (.>=), (.<=), not_
  , printS, printN, scanN, if_, newRef, getRef, bye
  , inj, wc, Grape.var, val, (~>), with, match, new
  , Grape.compile, Grape.compileAndRun
  ) where
import Pat hiding (Exp)
import Exp
import Stm hiding (Bind)
import Comp
import Control.Monad
import Control.Shell
import Data.Proxy -- for wc

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

if_ :: Exp Bool -> Stm (Exp a) -> Stm (Exp a) -> Stm (Exp a)
if_ = If

newRef :: Exp a -> Stm (Var a)
newRef = NewRef

getRef :: Var a -> Stm (Exp a)
getRef = Get

setRef :: Var a -> Exp a -> Stm ()
setRef = Set

bye :: Stm (Exp a)
bye = Die

newtype Bind a = Bind {val :: Exp a}

var :: ADT Stm a => Bind a -> a
var (Bind (Var v)) = Exp.var v

with :: (Bind a -> Stm b) -> Stm b
with f = do
  x <- newRef undef
  f (Bind $ Var x)

instance ADT Stm (Exp Int) where encAlg = Pat.Prim
instance ADT Stm (Exp Bool) where encAlg = Pat.Prim . B2I

instance ADT Stm Int where encAlg = Pat.Prim . Const
instance ADT Stm Bool where encAlg = Pat.Prim . B2I . Bool

instance ADT Stm a => ADT Stm (Maybe a)
instance (ADT Stm a, ADT Stm b) => ADT Stm (Either a b)

-- | An unnamed wildcard.
wc :: ADT Stm a => a
wc = wcByProxy (Proxy :: Proxy Stm)
