{-# LANGUAGE GADTs, TypeOperators, FlexibleInstances, FlexibleContexts, DefaultSignatures, ScopedTypeVariables, TypeFamilies, MultiParamTypeClasses #-}
module Pat where
import Control.Exception
import Data.Hashable
import GHC.Generics
import System.IO.Unsafe
import Control.Monad
import Data.Typeable
import Data.Word

type Name = Int
type Tag = Int

data Algebraic where
  Alg :: Typeable m => TypeRep -> Alg m -> Algebraic

-- | Encoding of an algebraic type or pattern: can be either a primitive value,
--   an algebraic constructor + arguments, or an optionally named hole.
--   Holes are only OK in patterns.
data Alg (m :: * -> *) where
  Prim :: Prim m -> Alg m
  Con  :: Tag -> [Alg m] -> Alg m
  Hole :: Maybe Name -> Alg m -- TODO: Name is stupid; generalize!

type Offset = Int

-- | How much memory is needed by the constructor and pointers to arguments?
allocSize :: Alg m -> Int
allocSize (Prim _)   = 1
allocSize (Con _ as) = length as + 1
allocSize (Hole _)   = error "holes have no size, silly"

-- TODO: this could probably be a lot smaller
class Monad m => PatM m where
  type Exp m  :: * -> *
  type Prim m :: *
  tagToPrim   :: Tag -> m (Prim m)
  unwrap      :: ADT m a => Exp m a -> m (Prim m)
  alloc       :: Int -> (Prim m -> m ()) -> m (Exp m a)
  store       :: Prim m -> Offset -> Prim m -> m ()
  load        :: Prim m -> Offset -> m (Prim m)
  ifThenElse  :: Exp m Bool -> m (Exp m a) -> m (Exp m a) -> m (Exp m a)
  equals      :: Prim m -> Prim m -> m (Exp m Bool)
  conjunction :: [Exp m Bool] -> m (Exp m Bool)
  bool        :: Bool -> m (Exp m Bool)
  setRef      :: Name -> Prim m -> m ()
  die         :: String -> m (Exp m a)

data PatEx where
  PatEx :: Typeable m => Alg m -> PatEx
instance Show PatEx where
  show _ = "PatEx"
instance Exception PatEx

-- | Encode an ADT as a pattern.
encode :: forall (m :: * -> *) a. ADT m a => a -> Alg m
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

class Typeable m => ADT m a where
  encAlg :: a -> Alg m
  default encAlg :: (Generic a, GAlg m (Rep a)) => a -> Alg m
  encAlg = head . algG . from

class GAlg m f where
  algG :: f a -> [Alg m]

-- Constructor without arguments: M1 C case takes care of this
instance GAlg m U1 where
  algG U1 = []

-- Data constructor metadata: a value begins here
instance (GAlg m a, Constructor c) => GAlg m (M1 C c a) where
  algG (M1 x) = [Con (hash $ conName (undefined :: M1 C c a ())) (algG x)]

-- Heap of data type metadata
instance GAlg m a => GAlg m (M1 D c a) where
  algG (M1 x) = algG x

-- Record selector metadata
instance GAlg m a => GAlg m (M1 S c a) where
  algG (M1 x) = algG x

-- Primitive/value with kind *
instance ADT m a => GAlg m (K1 i a) where
  algG (K1 x) = [encode x]

-- Sum type
instance (GAlg m a, GAlg m b) => GAlg m (a :+: b) where
  algG (L1 x) = algG x
  algG (R1 x) = algG x

-- Product type
instance (GAlg m a, GAlg m b) => GAlg m (a :*: b) where
  algG (a :*: b) = algG a ++ algG b


-- Constructing and expecting ADTs

-- | Pattern representation.
newtype Pat m a = Pat {unPat :: Alg m}

-- | A case in a pattern match.
type Case m a b = (Pat m a, m (Exp m b))

(~>) :: (PatM m, ADT m a) => a -> m (Exp m b) -> Case m a b
a ~> b = (pat a, b)
infixr 0 ~>

new :: forall m a. (PatM m, ADT m a) => a -> m (Exp m a)
new = store' . encAlg
  where
    store' :: Alg m -> m (Exp m a)
    store' alg = do
      alloc (allocSize alg) $ \ptr -> go ptr alg

    go ptr (Prim p) = store ptr 0 p
    go ptr (Con t as) = do
      store ptr 0 =<< tagToPrim t
      ptrs <- mapM (store' >=> unwrap) as
      zipWithM_ (store ptr) [1..] ptrs
    go _ (Hole _) = do
      error "can't store holes, silly"

matchOne :: PatM m => Prim m -> Alg m -> Int -> m (Exp m Bool)
matchOne ptr pat off = do
  case pat of
    Prim p -> do
      x <- load ptr off
      equals x p
    Hole Nothing -> do
      bool True
    Hole (Just n) -> do
      load ptr off >>= setRef n >> bool True
    Con t as -> do
      t' <- load ptr off
      eq <- equals t' =<< tagToPrim t
      flip (ifThenElse eq) (bool False) $ do
        ress <- forM (zip as [off+1 ..]) $ \(pat', off') -> do
          ptr' <- load ptr off'
          matchOne ptr' pat' 0
        conjunction ress

match :: (PatM m, ADT m a) => Exp m a -> [Case m a b] -> m (Exp m b)
match scrut ((Pat p, s):cs) = do
  scrut' <- unwrap scrut
  matches <- matchOne scrut' p 0
  ifThenElse matches s (match scrut cs)
match _ _ = do
  die "incomplete pattern match"


-- Building patterns and injecting terms

-- | Build a pattern from an ADT.
pat :: ADT m a => a -> Pat m a
pat = Pat . encode

-- | An unnamed wildcard, for the monad determined by the given proxy.
wcByProxy :: forall m a. ADT m a => Proxy m -> a
wcByProxy _ = throw $ PatEx (Hole Nothing :: Alg m)
