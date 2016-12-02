{-# LANGUAGE GADTs, TypeOperators, FlexibleInstances, FlexibleContexts, DefaultSignatures, ScopedTypeVariables, TypeFamilies #-}
module Pat where
import Control.Exception
import Data.Hashable
import GHC.Generics
import System.IO.Unsafe
import Control.Monad

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

data Ptr
type Offset = Int

-- | How much memory is needed by the constructor and pointers to arguments?
allocSize :: Alg -> Int
allocSize (Prim _)   = 1
allocSize (Con _ as) = length as + 1
allocSize (Hole _)   = error "holes have no size, silly"

-- TODO: this could probably be a lot smaller
class Monad m => PatM m where
  type Exp m :: * -> *
  tagToPrim   :: Tag -> m (Exp m PrimType)
  primToExp   :: PrimType -> m (Exp m PrimType)
  wrap        :: ADT a => Exp m Ptr -> m (Exp m a)
  unwrap      :: ADT a => Exp m a -> m (Exp m Ptr)
  alloc       :: Int -> m (Exp m Ptr)
  store       :: Exp m Ptr -> Offset -> Exp m a -> m ()
  load        :: Exp m Ptr -> Offset -> m (Exp m a)
  ifThenElse  :: Exp m Bool -> m (Exp m a) -> m (Exp m a) -> m (Exp m a)
  equals      :: Exp m PrimType -> Exp m PrimType -> m (Exp m Bool)
  conjunction :: [Exp m Bool] -> m (Exp m Bool)
  bool        :: Bool -> m (Exp m Bool)
  setRef      :: Name -> Exp m a -> m ()
  die         :: String -> m (Exp m a)

data PatEx = PatEx Alg
instance Show PatEx where
  show _ = "PatEx"
instance Exception PatEx

-- | Encode an ADT as a pattern.
encode :: ADT a => a -> Alg
encode x = unsafePerformIO $ do
  epat <- try $ pure $! encAlg $! x
  case epat of
    Left (PatEx x') -> pure x'
    Right x'        -> pure x'

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
  algG (K1 x) = [encode x]

-- Sum type
instance (GAlg a, GAlg b) => GAlg (a :+: b) where
  algG (L1 x) = algG x
  algG (R1 x) = algG x

-- Product type
instance (GAlg a, GAlg b) => GAlg (a :*: b) where
  algG (a :*: b) = algG a ++ algG b


-- Constructing and expecting ADTs

-- | Pattern representation.
newtype Pat a = Pat {unPat :: Alg}

-- | A case in a pattern match.
type Case m a b = (Pat a, m (Exp m b))

(~>) :: (PatM m, ADT a) => a -> m (Exp m b) -> Case m a b
a ~> b = (pat a, b)
infixr 0 ~>

new :: (PatM m, ADT a) => a -> m (Exp m a)
new x = do
    ptr <- store' (encAlg x)
    wrap ptr
  where
    store' alg = do
      ptr <- alloc $ allocSize alg
      go ptr alg
      return ptr

    go ptr (Prim p) = store ptr 0 =<< primToExp p
    go ptr (Con t as) = do
      store ptr 0 =<< tagToPrim t
      ptrs <- mapM store' as
      zipWithM_ (store ptr) [1..] ptrs
    go _ (Hole _) = do
      error "can't store holes, silly"

matchOne :: PatM m => Exp m Ptr -> Alg -> Int -> m (Exp m Bool)
matchOne ptr pat off = do
  case pat of
    Prim p -> do
      x <- load ptr off
      equals x =<< primToExp p
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

match :: (PatM m, ADT a) => Exp m a -> [Case m a b] -> m (Exp m b)
match scrut ((Pat p, s):cs) = do
  scrut' <- unwrap scrut
  matches <- matchOne scrut' p 0
  ifThenElse matches s (match scrut cs)
match _ _ = do
  die "incomplete pattern match"


-- Building patterns and injecting terms

-- | Build a pattern from an ADT.
pat :: ADT a => a -> Pat a
pat = Pat . encode

-- | An unnamed wildcard.
wc :: ADT a => a
wc = throw $ PatEx $ Hole Nothing
