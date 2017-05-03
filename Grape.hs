{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, GADTs #-}
-- | Grape: Generic Reification of Algebraics and Patterns for EDSLs
module Grape
  ( Algebraic (..), Exp, Stm, Pat, Var, Bind, Stm.Alg
  , module Control.Monad
  , true, false, undef, (.==), (!=), (.>), (.<), (.>=), (.<=), not_
  , printS, printN, scanN, if_, newRef, getRef
  , inj, wc, Grape.var, val, (~>), with, match', matchDef, new
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

newRef :: Exp a -> Stm (Var a)
newRef = NewRef

getRef :: Var a -> Stm (Exp a)
getRef = Get

setRef :: Var a -> Exp a -> Stm ()
setRef = Set

newtype Bind a = Bind (AlgRef a)

var :: (Embed Stm a ~ Prim Stm, Ref Stm (Prim Stm) ~ AlgRef t, Typeable a)
    => Bind t -> a
var (Bind v) = Stm.primVar v

varA :: (RefM Stm (ADT Stm a), Ref Stm (ADT Stm a) ~ AlgRef t, Typeable a)
    => Bind t -> a
varA (Bind v) = Stm.algVar v

val :: Bind a -> a
val (Bind (AlgRef v))  = Alg (unsafeFreeze v)
val (Bind (PrimRef v)) = unsafeFreeze v

class With a where
  with :: (Bind a -> Stm b) -> Stm b

instance With (Stm.Alg a) where
  with f = do
    v <- newRef undef
    f (Bind $ AlgRef v)

instance With (Exp Int) where
  with f = do
    v <- newRef undef
    f (Bind $ PrimRef v)

-- | An unnamed wildcard.
wc :: Algebraic Stm a => a
wc = wcFor (Proxy :: Proxy Stm)
