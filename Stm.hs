{-# LANGUAGE GADTs, TypeFamilies, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
-- | Statement language: side effects and conditionals
module Stm where
import Exp
import Pat hiding (Exp, Alg)
import qualified Pat
import Control.Monad
import Data.Proxy
import Control.Exception
import Data.Typeable
import Data.Dynamic

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

instance If Stm (Exp Int) (Exp a) where
  if_ c = If (i2b c)

instance If Stm (Exp Bool) (Alg a) where
  if_ c a b = Alg <$> If c (unAlg <$> a) (unAlg <$> b)
    where unAlg (Alg x) = x

instance If Stm (Exp Bool) () where
  if_ c a b = If c (a >> pure Undef) (b >> pure Undef) >> pure ()

type instance Embed Stm Int  = Exp Int
type instance Embed Stm Bool = Exp Bool

instance Algebraic Stm (Exp Int) where
  encode = encodePrim
  match = matchPrim

instance Algebraic Stm (Exp Bool) where
  encode = encodePrim . B2I
  match = matchPrim . B2I

instance Algebraic Stm Int where
  encode = encodePrim . Const
  match = matchPrim . Const

instance Algebraic Stm Bool where
  encode = encode . Bool
  match = matchPrim . B2I . Bool


-- | IMPORANT: the code generator of the example EDSL treats pointers as ints
--   except when dereferencing, so pointer arithmetic must happen in increments
--   of word size, not elements!
wordSize :: Exp Int
wordSize = 8

data AlgRef a where
  AlgRef  :: Var Int -> AlgRef (Alg a)
  PrimRef :: Var Int -> AlgRef (Exp Int)

instance RefM Stm (Exp Int) where
  type Ref Stm (Exp Int) = AlgRef (Exp Int)
  setRef (PrimRef r) = Set r

instance RefM Stm (Alg a) where
  type Ref Stm (Alg a) = AlgRef (Alg a)
  setRef (AlgRef r) (Alg x) = Set r x

type Ptr = Exp Int
data Alg a = Alg Ptr

instance Pat.PatM Stm where
  type ADT Stm  = Alg
  type Prim Stm = Exp Int
  alloc xs = do
    ptr <- Alloca (length xs)
    sequence_ [Write ptr i x | (i, x) <- zip [0..] xs]
    return (Alg ptr)
  equals a b = pure $ b2i $ a .== b
  index (Alg ptr) off = Read ptr off
  slice (Alg ptr) off = pure $ Alg (ptr + fromIntegral off*wordSize)
  fromInt _ = fromIntegral

-- | Inject an EDSL term into an ADT.
inj :: Algebraic Stm (Embed Stm a) => Embed Stm a -> a
inj = injFor (Proxy :: Proxy Stm)

-- | A named wildcard.
primVar :: (Embed Stm a ~ Prim Stm, Typeable a, RefM Stm (Prim Stm)) => Ref Stm (Prim Stm) -> a
primVar = primVarFor (Proxy :: Proxy Stm)

-- | A named wildcard.
algVar :: (Typeable a, RefM Stm (ADT Stm a)) => Ref Stm (ADT Stm a) -> a
algVar = algVarFor (Proxy :: Proxy Stm)
