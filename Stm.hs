{-# LANGUAGE GADTs, TypeFamilies, FlexibleContexts #-}
-- | Statement language: side effects and conditionals
module Stm where
import Exp
import Pat hiding (Exp, Alg)
import qualified Pat
import Control.Monad
import Data.Proxy
import Control.Exception

data Stm a where
  -- Monad ops
  Return :: a -> Stm a
  Bind   :: Stm a -> (a -> Stm b) -> Stm b

  -- I/O
  Print  :: String -> Stm ()
  PrintN :: Exp Int -> Stm ()
  Scan   :: Stm (Exp Int)

  -- Refs
  Set    :: Var a -> Exp a -> Stm ()
  Get    :: Var a -> Stm (Exp a)
  NewRef :: Exp a -> Stm (Var a)

  -- Loading/storing things; offsets are in # of machine words
  Read   :: Exp Int -> Int -> Stm (Exp a)
  Write  :: Exp Int -> Int -> Exp a -> Stm ()
  Alloca :: Int -> Stm (Exp Int)

  -- Conditionals
  If     :: Exp Bool -> Stm (Exp a) -> Stm (Exp a) -> Stm (Exp a)

  -- Untimely termination
  Die    :: Stm (Exp a)

instance Functor Stm where
  fmap f m = m >>= return . f

instance Applicative Stm where
  (<*>) = ap
  pure  = return

instance Monad Stm where
  return = Return
  (>>=)  = Bind

instance Pat.PatM Stm where
  type Exp Stm  = Exp
  type Prim Stm = Exp Int
  type Name Stm = Int
  unwrap (Alg (V n)) = pure (Var (V n))
  unwrap (Var (V n)) = pure (Var (V n))
  alloc n f = do
    ptr@(Var v) <- Alloca n
    f ptr
    return (Alg v)
  store = Write
  load = Read
  ifThenElse = If
  equals a b = pure $ a .== b
  conjunction = pure . foldr (.&&) true
  bool = pure . Bool
  setRef v x = Set (V v) x
  die s = Print s >> Die

-- | Inject an EDSL term into an ADT.
inj :: ADT Stm (Exp a) => Exp a -> a
inj = throw . PatEx . encAlgFor (Proxy :: Proxy Stm)

-- | A named wildcard.
var :: ADT Stm a => Var a -> a
var = throw . PatEx . hole (Proxy :: Proxy Stm) . Just . varName
