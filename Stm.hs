{-# LANGUAGE GADTs, TypeFamilies, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
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

instance Functor Stm where
  fmap f m = m >>= return . f

instance Applicative Stm where
  (<*>) = ap
  pure  = return

instance Monad Stm where
  return = Return
  (>>=)  = Bind

instance If (Exp Int) (Stm (Exp a)) where
  if_ c = If (i2b c)

type instance Term Stm a = Exp a

instance Algebraic Stm (Exp Int) where
  encode = encodePrim

-- | IMPORANT: the code generator of the example EDSL treats pointers as ints
--   except when dereferencing, so pointer arithmetic must happen in increments
--   of word size, not elements!
wordSize :: Exp Int
wordSize = 8

instance Pat.PatM Stm where
  type ADT Stm  = Exp
  type Prim Stm = Exp Int
  type Name Stm = Int
  alloc xs = do
    ptr@(Var v) <- Alloca (length xs)
    sequence_ [Write ptr i x | (i, x) <- zip [0..] xs]
    return (Alg v)
  equals a b = pure $ b2i $ a .== b
  setRef v x = Set (V v) x
  index (Alg v) off = Read (Var v) off
  index (Var (V v)) off = index (Alg (V v)) off
  slice (Alg v) off = do
    v' <- NewRef (Var v + fromIntegral off*wordSize)
    pure $ Alg v'
  slice (Var (V v)) off = slice (Alg (V v)) off

-- | Inject an EDSL term into an ADT.
inj :: Algebraic Stm (Exp a) => Exp a -> a
inj = injFor (Proxy :: Proxy Stm)

-- | A named wildcard.
var :: Algebraic Stm a => Var a -> a
var = untypedVarFor (Proxy :: Proxy Stm) . varName

{-
TODO: setRef will currently write pointer even when we're looking for primitive
value! we could handle by doing alloc [x] = Alg x, but that would be weird and
force ADT to behave as Either Array Prim, which is just weird.
figure out how to write &alg to ref when needed, alg when needed
-}
