{-# LANGUAGE GADTs, TypeOperators, FlexibleInstances, FlexibleContexts, DefaultSignatures, ScopedTypeVariables, TypeFamilies #-}
module Pat where
import Control.Exception
import Data.Hashable
import GHC.Generics
import System.IO.Unsafe

type Name = Int
type Tag = Int

-- | Decouple 'Alg' from any particular implementation without having to pass
--   the implementation as a type var.
data family PrimType

-- | Encoding of an algebraic type or pattern: can be either a primitive value,
--   an algebraic constructor + arguments, or an optionally named hole.
--   Holes are only OK in patterns.
--   TODO: turn into a GADT and forcefully restrict holes to patterns?
data Alg = Prim PrimType | Con Tag [Alg] | Hole (Maybe Name)

data PatEx = PatEx Alg
instance Show PatEx where
  show _ = "PatEx"
instance Exception PatEx

class ADT a where
  encAlg :: a -> Alg
  default encAlg :: (Generic a, GAlg (Rep a)) => a -> Alg
  encAlg = head . algG . from

class GAlg f where
  algG :: f a -> [Alg]

-- Constructor without arguments: M1 C case takes care of this
instance GAlg U1 where
  algG U1 = []

-- Data constructor metadata: a value begins here
instance (GAlg a, Constructor c) => GAlg (M1 C c a) where
  algG (M1 x) = [Con (hash $ conName (undefined :: M1 C c a ())) (algG x)]

-- Heap of data type metadata
instance GAlg a => GAlg (M1 D c a) where
  algG (M1 x) = algG x

-- Record selector metadata
instance GAlg a => GAlg (M1 S c a) where
  algG (M1 x) = algG x

-- Primitive/value with kind *
instance ADT a => GAlg (K1 i a) where
  algG (K1 x) = unsafePerformIO $ do
    epat <- try $ pure $! encAlg $! x
    case epat of
      Left (PatEx x') -> pure [x']
      Right x'        -> pure [x']

-- Sum type
instance (GAlg a, GAlg b) => GAlg (a :+: b) where
  algG (L1 x) = algG x
  algG (R1 x) = algG x

-- Product type
instance (GAlg a, GAlg b) => GAlg (a :*: b) where
  algG (a :*: b) = algG a ++ algG b
