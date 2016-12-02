{-# LANGUAGE FlexibleInstances #-}
-- | Grape: Generic Reification of ADTs and Patterns for EDSLs
module Grape
  ( ADT (..), Exp, Stm, Pat, Var
  , module Control.Monad
  , true, false, undef, (.==), (!=), (.>), (.<), (.>=), (.<=), not_
  , printS, printN, scanN, if_, newRef, getRef, bye
  , inj, wc, var, (~>), with, match, new
  , Trope.compile, Trope.compileAndRun
  ) where
import Pat
import Exp
import Stm
import Comp
import Control.Monad
import Control.Shell

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

with :: ((Var a, Exp a) -> Stm b) -> Stm b
with f = do
  x <- newRef undef
  f (x, Var x)

instance ADT (Exp Int) where encAlg = Prim . PInt
instance ADT (Exp Bool) where encAlg = Prim . PBool

instance ADT Int where encAlg = Prim . PInt . Const
instance ADT Bool where encAlg = Prim . PBool . Bool

instance ADT a => ADT (Maybe a)
instance (ADT a, ADT b) => ADT (Either a b)
