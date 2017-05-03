{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables, UndecidableInstances #-}
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
  ( Typeable, Dynamic, Algebraic (..), PatM (..), RefM (..), If (..), GAlg (..)
  , Alg, Pat, Case, Embed
  , match', matchDef, (~>), new
  , encodePrim, matchPrim
  , pat, injFor, wcFor, primVarFor, algVarFor
  ) where
import Control.Exception
import Control.Monad
import Data.Dynamic
import Data.Proxy
import Data.Typeable
import GHC.Generics
import System.IO.Unsafe

import Debug.Trace

-- | Encoding of an algebraic value: can be either a primitive value or
--   an algebraic constructor + arguments.
data Alg (m :: * -> *) where
  Prim :: Prim m -> Alg m
  Con  :: Prim m -> [Alg m] -> Alg m

type Offset = Int
type ConTag = Int

flatten :: Alg m -> [Prim m]
flatten (Prim p)   = [p]
flatten (Con t as) = t : concatMap flatten as

-- | How much memory is needed by the constructor and pointers to arguments?
size :: Alg m -> Int
size (Prim _)   = 1
size (Con _ as) = sum (map size as) + 1

class If m c a where
  if_ :: c -> m a -> m a -> m a

class RefM m a where
  -- | A typed reference in the client language.
  type Ref m a :: *

  -- | Write a value to a reference.
  setRef :: Ref m a -> a -> m ()

class ( Monad m
      , Typeable m
      , Typeable (Prim m)
      , Typeable (ADT m)
      , RefM m (Prim m)
      , If m (Prim m) (Prim m)
      ) => PatM m where
  -- | The type of algebraic values in the client language.
  type ADT m  :: * -> *

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

  -- | Convert an into the client language's primitive type.
  fromInt     :: Proxy m -> Int -> Prim m

  -- | Boolean conjunction over the language's primitive type.
  --   As with 'equals', @0@/@1@ is used to represent @False@/@True@.
  conjunction :: [Prim m] -> m (Prim m)
  conjunction (x:xs) = do
    res <- x `equals` (fromInt (Proxy :: Proxy m) 0)
    if_ res (pure (fromInt (Proxy :: Proxy m) 0)) (conjunction xs)
  conjunction [] = do
    pure (fromInt (Proxy :: Proxy m) 1)

true, false :: forall m. PatM m => Proxy m -> Prim m
true p = fromInt p 1
false p = fromInt p 0

data Hole m a where
  Wildcard :: Hole m a
  PrimHole :: (Embed m a ~ Prim m, RefM m (Prim m)) => Ref m (Prim m) -> Hole m a
  AlgHole  :: RefM m (ADT m a) => Ref m (ADT m a) -> Hole m a
instance Show (Hole m a) where
  show (Wildcard)   = "Wildcard"
  show (PrimHole _) = "PrimHole *"
  show (AlgHole _)  = "AlgHole *"
instance (Typeable m, Typeable a) => Exception (Hole m a)

data Inj m where
  Inj :: Alg m -> Inj m
instance Show (Inj m) where
  show _ = "Inj"
instance Typeable m => Exception (Inj m)

-- | Encode an ADT as a pattern.
enc :: forall (m :: * -> *) a. Algebraic m a => a -> Alg m
enc x = unsafePerformIO $ do
  ealg <- try $ pure $! encode $! x
  case ealg of
    Right x'        -> pure x'
    Left e@(Inj x') -> pure x'

class (Typeable a, PatM m) => Algebraic m a where
  encode :: a -> Alg m
  match :: a -> ADT m a -> m (Prim m)
  len :: Proxy m -> a -> Int
  len _ _ = 1

instance {-# OVERLAPPABLE #-} (Typeable a, PatM m, Generic a, GAlg m (Rep a)) => Algebraic m a where
  encode = head . flip algG 0 . from
  match pat ptr = matchG ptr (from pat) 0 0
  len p = lenG p . from

-- | Match a value of the primitive type for the language @m@.
matchPrim :: PatM m => Prim m -> ADT m a -> m (Prim m)
matchPrim x ptr = index ptr 0 >>= equals x

-- | Encode a value of the primitive type for the language @m@.
encodePrim :: Prim m -> Alg m
encodePrim = Prim

-- | Encode an algebraic value for the given EDSL monad.
encodeFor :: Algebraic m a => Proxy m -> a -> Alg m
encodeFor _ = encode  

class PatM m => GAlg m f where
  algG :: f a -> ConTag -> [Alg m]
  lenG :: Proxy m -> f a -> Int
  matchG :: Typeable a => ADT m a -> f t -> ConTag -> Offset -> m (Prim m)

instance PatM m => GAlg m U1 where
  algG U1 _ = []
  lenG _ U1 = 0
  matchG _ U1 _ _ = pure (true (Proxy :: Proxy m))

-- Data constructor metadata: a value begins here
instance (GAlg m a, Constructor c) => GAlg m (M1 C c a) where
  algG (M1 x) tid = [Con (fromInt (Proxy :: Proxy m) tid) (algG x 0)]
  lenG p (M1 x) = 1 + lenG p x
  matchG ptr (M1 x) tid off = do
      con <- index ptr off
      eq <- con `equals` (fromInt p tid)
      if_ eq (matchG ptr x tid (off+1)) (pure (false p))
    where
      p = Proxy :: Proxy m
      
-- Uninteresting data type metadata
instance {-# OVERLAPPABLE #-} GAlg m a => GAlg m (M1 t c a) where
  algG (M1 x) = algG x
  matchG ptr (M1 x) tid off = matchG ptr x tid off
  lenG p (M1 x) = lenG p x

-- Primitive/value with kind *
instance ( Algebraic m a
         ) => GAlg m (K1 i a) where
  algG (K1 x) _ = [enc x]
  matchG ptr (K1 x) _ off = do
      case checkInj p x of
        Value x'            -> slice ptr off >>= match x'
        HoleEx Wildcard     -> ok
        HoleEx (PrimHole r) -> index ptr off >>= setRef r >> ok
        HoleEx (AlgHole r)  -> (slice ptr off :: m (ADT m a)) >>= setRef r >> ok
        InjEx (Inj x')      -> same off ptr x'
    where
      ok = pure (true p)
      p = Proxy :: Proxy m
  lenG p (K1 x) = len p x

-- | Compare an ADT to a pattern that's guaranteed to contain neither holes
--   nor injections.
same :: forall m a. PatM m => Offset -> ADT m a -> Alg m -> m (Prim m)
same off x (Prim p)   = index x off >>= equals p
same off x (Con t as) = do
  eq <- index x off >>= equals t
  if_ eq
    (conjunction =<< matchArgs (off+1) as)
    (pure $ false (Proxy :: Proxy m))
  where
    matchArgs off' (a:as) = do
      eq <- same off' x a
      eqs <- matchArgs (off'+1) as
      return (eq:eqs)
    matchArgs _ _ = do
      return []


-- | Force a value to WHNF, catching injection and hole exceptions.
checkInj :: forall m a. Algebraic m a => Proxy m -> a -> InjVal m a
checkInj _ x = unsafePerformIO $ do
  (fmap Value . pure $! x) `catches`
    [ Handler $ return . HoleEx
    , Handler $ return . InjEx
    ]

-- | A value with possible injections or holes.
data InjVal m a
  = Value a
  | InjEx (Inj m)
  | HoleEx (Hole m a)

-- Sum type
instance (GAlg m a, GAlg m b) => GAlg m (a :+: b) where
  algG (L1 x) tid = algG x (tid*2)
  algG (R1 x) tid = algG x (1+tid*2)
  matchG ptr (L1 x) tid off = matchG ptr x (tid*2) off
  matchG ptr (R1 x) tid off = matchG ptr x (1+tid*2) off
  lenG p (L1 x) = lenG p x
  lenG p (R1 x) = lenG p x

-- Product type
instance (GAlg m a, GAlg m b) => GAlg m (a :*: b) where
  algG (a :*: b) _ = algG a 0 ++ algG b 0
  matchG ptr (a :*: b) tid off = do
    a' <- matchG ptr a 0 off
    b' <- matchG ptr b 0 (off + lenG (Proxy :: Proxy m) a)
    conjunction [a', b']
  lenG p (a :*: b) = lenG p a + lenG p b


-- Constructing and inpecting ADTs

-- | Pattern representation.
newtype Pat (m :: * -> *) a = Pat {unPat :: a}

-- | A case in a pattern match.
type Case m a b = (Pat m a, m b)

(~>) :: Algebraic m a => a -> m b -> Case m a b
a ~> b = (pat a, b)
infixr 0 ~>

-- | Create a new algebraic value.
new :: forall m a. Algebraic m a => a -> m (ADT m a)
new = alloc . (flatten :: Alg m -> [Prim m]) . encode

-- | Match with default; if no pattern matches, the first argument is returned.
matchDef :: forall m a b. (Algebraic m a, If m (Prim m) b) => b -> ADT m a -> [Case m a b] -> m b
matchDef def scrut cases = do
    go scrut cases
  where
    ok = pure $ true (Proxy :: Proxy m)
    go ptr ((Pat p, s) : cs) = do
      matches <- case checkInj (Proxy :: Proxy m) p of
        Value p'            -> slice ptr 0 >>= match p'
        HoleEx Wildcard     -> ok
        HoleEx (PrimHole r) -> index ptr 0 >>= setRef r >> ok
        HoleEx (AlgHole r)  -> setRef r ptr >> ok
      if_ matches s (go ptr cs)
    go _ _ = do
      return def

-- | Match without default; no value is returned, so non-exhaustive patterns
--   result in a no-op.
match' :: forall m a b. Algebraic m a => ADT m a -> [Case m a b] -> m ()
match' scrut = void . matchDef false scrut . map (fmap (>> pure false))
  where false = fromInt (Proxy :: Proxy m) 0

-- Building patterns and injecting terms

-- | Build a pattern from an ADT.
pat :: Algebraic m a => a -> Pat m a
pat = Pat

-- | An unnamed wildcard, for the monad determined by the given proxy.
wcFor :: forall m a. Algebraic m a => Proxy m -> a
wcFor _ = throw (Wildcard :: Hole m a)

-- | Inject a basic EDSL term into an ADT.
injFor :: Algebraic m (Embed m a) => Proxy m -> Embed m a -> a
injFor p = throw . Inj . encodeFor p

-- | Inject a named primitive wildcard for the given EDSL.
--   The proxy gives the type of the language.
primVarFor :: forall m a.
              (Typeable m, Typeable a, RefM m (Prim m), Embed m a ~ Prim m)
           => Proxy m
           -> Ref m (Prim m)
           -> a
primVarFor _ r = throw ((PrimHole r) :: Hole m a)

-- | Inject a named algebraic wildcard for the given EDSL.
--   The proxy gives the type of the language.
algVarFor :: forall m a.
             (Typeable m, Typeable a, RefM m (ADT m a))
           => Proxy m
           -> Ref m (ADT m a)
           -> a
algVarFor _ r = throw ((AlgHole r) :: Hole m a)

-- | The @m@-embedded equivalent type of the Haskell type @a@.
--   For instance, if a language @L@ represents integers as @Exp Int@, then
--   the language would need an instance @Embed L Int = Exp Int@.
--
--   As a rule of thumb, you should have one instance of @Embed@ for each
--   instance of @Algebraic@.
type family Embed (m :: * -> *) a
