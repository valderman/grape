{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-} -- For Show instances
-- | Duct-taping ADTs and pattern matching onto arbitrary monadic EDSLs.
--   To get pattern matching, an EDSL needs to do the following:
--
--     * Add the appropriate type family instances for 'Term'.
--     * Instantiate 'If'.
--     * Instantiate 'PatM'.
--     * Instantiate 'Num' for the language's primitive type.
--     * Instantiate 'Algebraic' for each base type of the EDSL.
--       'encode' needs to encode any base type into the primitive type of the
--       language. Once you have a value of the primitive type, use 'encodePrim'
--       to encode it into the required 'Alg' type.
--
--   Note that the client language is responsible for generating 'Name's to bind
--   in pattern matches, as well as some way of extracting the value bound to
--   a name.
module Pat
  ( Typeable, Algebraic (..), PatM (..), If (..)
  , Alg, Pat, Case, Term
  , match', matchDef, (~>), new
  , encodePrim
  , pat, injFor, wcFor, untypedVarFor
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
  Hole :: Maybe (Bool, Name m) -> Alg m -- TODO: can we make this type safe somehow?

instance (Show (Name m), Show (Prim m)) => Show (Alg m) where
  show (Prim x)        = "Prim " ++ show x
  show (Con t as)      = "Con " ++ show t ++ " " ++ show as
  show (Hole (Just n)) = "Hole (Just " ++ show n ++ ")"
  show (Hole Nothing)  = "Hole Nothing"

type Offset = Int

flatten :: Alg m -> [Prim m]
flatten (Prim p)   = [p]
flatten (Con t as) = t : concatMap flatten as
flatten (Hole _)   = error "Can't flatten a hole!"

-- | How much memory is needed by the constructor and pointers to arguments?
size :: Alg m -> Int
size (Prim _)   = 1
size (Con _ as) = sum (map size as) + 1
size (Hole _)   = error "holes have no size, silly"

class If c a where
  if_ :: c -> a -> a -> a

class ( Monad m
      , Typeable m
      , Typeable (Prim m)
      , Num (Prim m)
      , If (Prim m) (m (Prim m))
      ) => PatM m where
  -- | The type of algebraic values in the language.
  --
  --   TODO: due to lack of type-level lambdas, ADT m a can't be, say, Exp (T a).
  --   Look into a fix!
  type ADT m  :: * -> *

  -- | An untyped reference in the client language.
  type Name m :: *

  -- | The primitive type of the client language.
  type Prim m :: *

  -- | Store a list of primitive values in the client language.
  alloc       :: [Prim m] -> m (ADT m a)

  -- | Extract a value from an ADT.
  --   This function should return the @n@th value of the list originally passed
  --   to @alloc@ to create the given ADT value.
  index       :: ADT m a -> Offset -> m (Prim m)

  -- | Drop the first @n@ elements of the given ADT value.
  slice       :: ADT m a -> Offset -> m (ADT m b)

  -- | Boolean comparison. @0@ represents @False@, and @1@ represents @True@.
  equals      :: Prim m -> Prim m -> m (Prim m)

  -- | Write an ADT value to an untyped reference.
  --   If the given boolean is @True@, the given ADT value contains only a
  --   single primitive.
  setRef      :: Bool -> Name m -> ADT m a -> m ()

  -- | Boolean conjunction over the language's primitive type.
  --   As with 'equals', @0@/@1@ is used to represent @False@/@True@.
  conjunction :: [Prim m] -> m (Prim m)
  conjunction (x:xs) = do
    res <- x `equals` (0 :: Prim m)
    if_ res (pure (0 :: Prim m)) (conjunction xs)
  conjunction [] = do
    pure (1 :: Prim m)

data PatEx where
  PatEx :: Typeable m => Alg m -> PatEx
instance Show PatEx where
  show _ = "PatEx"
instance Exception PatEx

-- | Encode an ADT as a pattern.
enc :: forall (m :: * -> *) a. Algebraic m a => a -> Alg m
enc x = unsafePerformIO $ do
  epat <- try $ pure $! encode $! x
  case epat of
    Right x' -> pure x'
    Left e@(PatEx x') ->
      case same x' (Proxy :: Proxy m)of
        (Just Refl) -> pure x'
        _           -> throw e

same :: forall m1 m2. (Typeable m1, Typeable m2)
     => Alg (m1 :: * -> *) -> Proxy (m2 :: * -> *) -> Maybe (m1 :~: m2)
same _ _ = eqT :: Maybe (m1 :~: m2)

-- | A term of type @a@ in the language @m@.
type family Term (m :: * -> *) a

class PatM m => Algebraic m a where
  encode :: a -> Alg m
  default encode :: (Generic a, GAlg m (Rep a)) => a -> Alg m
  encode = head . flip algG 0 . from
instance {-# OVERLAPPABLE #-} (PatM m, Generic a, GAlg m (Rep a)) => Algebraic m a

-- | Encode a value of the primitive type for the language @m@.
encodePrim :: (PatM m, Prim m ~ prim) => prim -> Alg m
encodePrim = Prim

-- | Encode an algebraic value for the given EDSL monad.
encodeFor :: Algebraic m a => Proxy m -> a -> Alg m
encodeFor _ = encode

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
  algG (K1 x) _ = [enc x]

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

-- | Create a new algebraic value.
new :: forall m a. (PatM m, Algebraic m a) => a -> m (ADT m a)
new = alloc . (flatten :: Alg m -> [Prim m]) . encode

matchOne :: forall m a. PatM m => ADT m a -> Alg m -> Int -> m (Prim m)
matchOne ptr pat off = do
  case pat of
    Prim p -> do
      x <- index ptr off
      equals x p
    Hole Nothing -> do
      pure true
    Hole (Just (isPrim, n)) -> do
      slice ptr off >>= setRef isPrim n
      pure true
    Con t as -> do
      t' <- index ptr off
      eq <- equals t' t
      if_ eq
        (matchArgs (off+1) as >>= conjunction)
        (pure false)
  where
    true = 1 :: Prim m
    false = 0 :: Prim m
    matchArgs off' (a:as) = do
      a' <- slice ptr off'
      x <- matchOne a' a 0
      xs <- matchArgs (off' + size a) as
      return (x:xs)
    matchArgs _ _ = do
      return []

-- | Match with default; if no pattern matches, the first argument is returned.
matchDef :: (Algebraic m a, Algebraic m b, If (Prim m) (m b)) => b -> ADT m a -> [Case m a b] -> m b
matchDef def scrut cases = do
    go scrut cases
  where
    go scrut' ((Pat p, s) : cs) = do
      matches <- matchOne scrut' p 0
      if_ matches s (go scrut' cs)
    go _ _ = do
      return def

-- | Match without default; no value is returned, so non-exhaustive patterns
--   result in a no-op.
match' :: forall m a b. (PatM m, Algebraic m a, Algebraic m (Prim m)) => ADT m a -> [Case m a b] -> m ()
match' scrut = void . matchDef false scrut . map (fmap (>> pure false))
  where false = 0 :: Prim m


-- Building patterns and injecting terms

-- | Build a pattern from an ADT.
pat :: Algebraic m a => a -> Pat m a
pat = Pat . enc

-- | An unnamed wildcard, for the monad determined by the given proxy.
wcFor :: forall m a. Algebraic m a => Proxy m -> a
wcFor _ = throw $ PatEx (Hole Nothing :: Alg m)

-- | Inject a basic EDSL term into an ADT.
injFor :: Algebraic m (Term m a) => Proxy m -> Term m a -> a
injFor p = throw . PatEx . encodeFor p

-- | Inject an untyped named wildcard for the given EDSL.
--   The proxy gives the type of the language, the boolean indicates whether
--   the injected hole is primitive or not.
untypedVarFor :: forall m a. (Typeable a, Algebraic m a) => Proxy m -> Bool -> Name m -> a
untypedVarFor _ prim n =
    throw . PatEx . (Hole :: Maybe (Bool, Name m) -> Alg m) . Just $ (prim, n)
