{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}  -- For 'encode'
{-# LANGUAGE UndecidableInstances #-} -- For Show instances
module Pat
  ( Algebraic (..), PatM (..)
  , ADT, PrimExp, AlgExp, Pat, Case
  , match', matchDef, (~>), new
  , pat, wcFor, injFor, untypedVarFor
  ) where
import Control.Exception
import GHC.Generics
import System.IO.Unsafe
import Control.Monad
import Data.Typeable
import Data.Proxy

-- | Encoding of an algebraic type or pattern: can be either a primitive value,
--   an algebraic constructor + arguments, or an optionally named hole.
--   Holes are only OK in patterns.
data Alg (m :: * -> *) where
  Prim :: Prim m -> Alg m
  Con  :: Prim m -> [Alg m] -> Alg m
  Hole :: Maybe (Name m) -> Alg m -- TODO: can we make this type safe somehow?

instance (Show (Name m), Show (PrimExp m)) => Show (Alg m) where
  show (Prim x)        = "Prim " ++ show x
  show (Con t as)      = "Con " ++ show t ++ " " ++ show as
  show (Hole (Just n)) = "Hole (Just " ++ show n ++ ")"
  show (Hole Nothing)  = "Hole Nothing"

type Offset = Int

flatten :: Alg m -> [PrimExp m]
flatten (Prim p)   = [p]
flatten (Con t as) = t : concatMap flatten as
flatten (Hole _)   = error "Can't flatten a hole!"

-- | How much memory is needed by the constructor and pointers to arguments?
size :: Alg m -> Int
size (Prim _)   = 1
size (Con _ as) = sum (map size as) + 1
size (Hole _)   = error "holes have no size, silly"

-- TODO: type class for if-then-else
class If c a where
  if_ :: c -> a -> a -> a

-- TODO: this could probably be a lot smaller
class (Typeable m, Num (PrimExp m), Monad m) => PatM m where
  type ADT m  :: * -> *
  type Name m :: *
  type Prim m :: *
  alloc       :: [Prim m] -> m (ADT m a)
  index       :: ADT m a -> Offset -> m (Prim m)
  slice       :: ADT m a -> Offset -> m (ADT m b)
  ifThenElse  :: If (Prim m) (m a) => Prim m -> m a -> m a -> m a
  equals      :: Prim m -> Prim m -> m (Prim m)
  setRef      :: Name m -> ADT m a -> m ()

  conjunction :: [Prim m] -> m (Prim m)
  conjunction (x:xs) = do
    res <- x `equals` (0 :: Prim m)
    ifThenElse res (pure (0 :: Prim m)) (conjunction xs)
  conjunction [] = do
    pure (1 :: PrimExp m)

data PatEx where
  PatEx :: Typeable m => Alg m -> PatEx
instance Show PatEx where
  show _ = "PatEx"
instance Exception PatEx

-- | Encode an ADT as a pattern.
encode :: forall (m :: * -> *) a. Algebraic m a => a -> Alg m
encode x = unsafePerformIO $ do
  epat <- try $ pure $! encAlg $! x
  case epat of
    Right x' -> pure x'
    Left e@(PatEx x') ->
      case same x' (Proxy :: Proxy m)of
        (Just Refl) -> pure x'
        _           -> throw e

same :: forall m1 m2. (Typeable m1, Typeable m2)
     => Alg (m1 :: * -> *) -> Proxy (m2 :: * -> *) -> Maybe (m1 :~: m2)
same _ _ = eqT :: Maybe (m1 :~: m2)

class PatM m => Algebraic m a where
  encAlg :: a -> Alg m
  default encAlg :: (Generic a, GAlg m (Rep a)) => a -> Alg m
  encAlg = head . flip algG 0 . from

instance {-# OVERLAPPABLE #-} (PatM m, PrimExp m ~ prim) => Algebraic m prim where
  encAlg = Prim

-- | Encode an algebraic value for the given EDSL monad.
encAlgFor :: Algebraic m a => Proxy m -> a -> Alg m
encAlgFor _ = encAlg

class PatM m => GAlg m f where
  algG :: f a -> Int -> [Alg m]

-- Constructor without arguments: M1 C case takes care of this
instance PatM m => GAlg m U1 where
  algG U1 _ = []

-- Data constructor metadata: a value begins here
instance (GAlg m a, Constructor c) => GAlg m (M1 C c a) where
  algG (M1 x) tid = [Con (fromIntegral tid) (algG x 0)]

-- Heap of data type metadata
instance GAlg m a => GAlg m (M1 D c a) where
  algG (M1 x) = algG x

-- Record selector metadata
instance GAlg m a => GAlg m (M1 S c a) where
  algG (M1 x) = algG x

-- Primitive/value with kind *
instance Algebraic m a => GAlg m (K1 i a) where
  algG (K1 x) _ = [encode x]

-- Sum type
instance (GAlg m a, GAlg m b) => GAlg m (a :+: b) where
  algG (L1 x) tid = algG x (tid*2)
  algG (R1 x) tid = algG x (1+tid*2)

-- Product type
instance (GAlg m a, GAlg m b) => GAlg m (a :*: b) where
  algG (a :*: b) _ = algG a 0 ++ algG b 0


-- Constructing and inpecting ADTs

-- | Pattern representation.
newtype Pat m a = Pat {unPat :: Alg m}

-- | A case in a pattern match.
type Case m a b = (Pat m a, m b)

(~>) :: Algebraic m a => a -> m b -> Case m a b
a ~> b = (pat a, b)
infixr 0 ~>

new :: forall m a. (PatM m, Algebraic m a) => a -> m (AlgExp m a)
new = alloc . (flatten :: Alg m -> [PrimExp m]) . encAlg

matchOne :: forall m a. PatM m => AlgExp m a -> Alg m -> Int -> m (PrimExp m)
matchOne ptr pat off = do
  case pat of
    Prim p -> do
      x <- index ptr off
      equals x p
    Hole Nothing -> do
      pure true
    Hole (Just n) -> do
      error "TODO: the holes"
    Con t as -> do
      t' <- index ptr off
      eq <- equals t' t
      ifThenElse eq
        (matchArgs (off+1) as >>= conjunction)
        (pure false)
  where
    true = 1 :: PrimExp m
    false = 0 :: PrimExp m
    matchArgs off' (a:as) = do
      a' <- slice ptr off'
      x <- matchOne a' a 0
      xs <- matchArgs (off' + size a) as
      return (x:xs)

-- | Match with default; if no pattern matches, the first argument is returned.
matchDef :: (PatM m, Algebraic m a) => Exp m b -> AlgExp m a -> [Case m a (Exp m b)] -> m (Exp m b)
matchDef def scrut cases = do
    go scrut cases
  where
    go scrut' ((Pat p, s) : cs) = do
      matches <- matchOne scrut' p 0
      ifThenElse matches s (go scrut' cs)
    go _ _ = do
      return def

-- | Match without default; no value is returned, so non-exhaustive patterns
--   result in a no-op.
match' :: forall m a b. (PatM m, Algebraic m a) => AlgExp m a -> [Case m a b] -> m ()
match' scrut = void . matchDef false scrut . map (fmap (>> pure false))
  where false = 0 :: PrimExp m


-- Building patterns and injecting terms

-- | Build a pattern from an ADT.
pat :: Algebraic m a => a -> Pat m a
pat = Pat . encode

-- | An unnamed wildcard, for the monad determined by the given proxy.
wcFor :: forall m a. Algebraic m a => Proxy m -> a
wcFor _ = throw $ PatEx (Hole Nothing :: Alg m)

-- | Inject a basic EDSL term into an ADT.
injFor :: Algebraic m (Exp m a) => Proxy m -> Exp m a -> a
injFor p = throw . PatEx . encAlgFor p

-- | Inject an untyped named wildcard for the given EDSL.
untypedVarFor :: forall m a. Algebraic m a => Proxy m -> Name m -> a
untypedVarFor _ = throw . PatEx . (Hole :: Maybe (Name m) -> Alg m) . Just
