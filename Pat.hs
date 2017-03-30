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
  Prim :: PrimExp m -> Alg m
  Con  :: PrimExp m -> [Alg m] -> Alg m
  Hole :: Maybe (Name m) -> Alg m -- TODO: can we make this type safe somehow?

instance (Show (Name m), Show (PrimExp m)) => Show (Alg m) where
  show (Prim x)        = "Prim " ++ show x
  show (Con t as)      = "Con " ++ show t ++ " " ++ show as
  show (Hole (Just n)) = "Hole (Just " ++ show n ++ ")"
  show (Hole Nothing)  = "Hole Nothing"

type Offset = Int

data ADT a

-- | How much memory is needed by the constructor and pointers to arguments?
allocSize :: Alg m -> Int
allocSize (Prim _)   = 1
allocSize (Con _ as) = length as + 1
allocSize (Hole _)   = error "holes have no size, silly"

type PrimExp m = Exp m (Prim m)
type AlgExp m a = Exp m (ADT a)

-- TODO: this could probably be a lot smaller
class (Typeable m, Num (PrimExp m), Monad m) => PatM m where
  type Exp m  :: * -> *
  type Name m :: *
  type Prim m :: *
  unwrap      :: Algebraic m a => AlgExp m a -> m (PrimExp m)
  alloc       :: Int -> (PrimExp m -> m ()) -> m (AlgExp m a)
  store       :: PrimExp m -> Offset -> PrimExp m -> m ()
  load        :: PrimExp m -> Offset -> m (PrimExp m)
  ifThenElse  :: PrimExp m -> m (Exp m a) -> m (Exp m a) -> m (Exp m a)
  equals      :: PrimExp m -> PrimExp m -> m (PrimExp m)
  setRef      :: Name m -> PrimExp m -> m ()

  conjunction :: [PrimExp m] -> m (PrimExp m)
  conjunction (x:xs) = do
    res <- x `equals` (0 :: PrimExp m)
    ifThenElse res (pure (0 :: PrimExp m)) (conjunction xs)
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
new = store' . encAlg
  where
    store' :: Alg m -> m (AlgExp m a)
    store' alg = do
      alloc (allocSize alg) $ \ptr -> go ptr alg

    go ptr (Prim p) = store ptr 0 p
    go ptr (Con t as) = do
      store ptr 0 t
      ptrs <- mapM (store' >=> unwrap) as
      zipWithM_ (store ptr) [1..] ptrs
    go _ (Hole _) = do
      error "can't store holes, silly"

matchOne :: forall m. PatM m => PrimExp m -> Alg m -> Int -> m (PrimExp m)
matchOne ptr pat off = do
  case pat of
    Prim p -> do
      x <- load ptr off
      equals x p
    Hole Nothing -> do
      pure true
    Hole (Just n) -> do
      load ptr off >>= setRef n >> pure true
    Con t as -> do
      t' <- load ptr off
      eq <- equals t' t
      flip (ifThenElse eq) (pure false) $ do
        ress <- forM (zip as [off+1 ..]) $ \(pat', off') -> do
          ptr' <- load ptr off'
          matchOne ptr' pat' 0
        conjunction ress
  where
    true = 1 :: PrimExp m
    false = 0 :: PrimExp m

-- | Match with default; if no pattern matches, the first argument is returned.
matchDef :: (PatM m, Algebraic m a) => Exp m b -> AlgExp m a -> [Case m a (Exp m b)] -> m (Exp m b)
matchDef def scrut cases = do
    scrut' <- unwrap scrut
    go scrut' cases
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
