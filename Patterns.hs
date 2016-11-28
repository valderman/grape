{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, FlexibleInstances, TypeOperators, DefaultSignatures, FlexibleContexts, GADTs, RankNTypes #-}
module Patterns where
import Control.Exception
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad
import Data.Hashable
import Data.Typeable
import GHC.Generics
import System.IO.Unsafe

type Con = String
type Name = Int
type Offset = Int
type OffM = StateT Offset IO
type RunM = StateT Name (Writer String)
data Ptr a

data Stm a where
  (:>>=) :: Stm a -> (a -> Stm b) -> Stm b
  Ret    :: a -> Stm a
  New    :: Stm (Var a)
  Set    :: Var a -> Exp a -> Stm ()
  SetPtr :: Exp (Ptr a) -> Offset -> Exp a -> Stm () -- TODO: generalize
  Case   :: Pattern p => Exp p -> [(p, Stm ())] -> Stm ()
  Print  :: Exp Int -> Stm ()

unsafeFreeze :: Var a -> Exp a
unsafeFreeze = Var

data Exp a where
  Lit :: Show a => a -> Exp a
  Var :: Var a -> Exp a
  Ptr :: Exp Offset -> Exp (Ptr a)
  Ix  :: Offset -> Exp a -- TODO: this is stupid, implement proper ptrs!

-- TODO: arithmetic expressions
instance (Show a, Num a) => Num (Exp a) where
  fromInteger = Lit . fromInteger

data Var a where
  V :: Name -> Var a

instance Monad Stm where
  (>>=) = (:>>=)
  return = Ret

instance Applicative Stm where
  pure = return
  (<*>) = ap

instance Functor Stm where
  fmap f m = m >>= pure . f

-- | Generate C for a statement.
run :: Stm a -> RunM a
run New = do
  n <- get
  put (succ n)
  tell $ concat ["int v", show n, ";\n"]
  pure (V n)
run (Set (V v) e) = do
  tell $ concat ["v", show v, " = "]
  runExp e
  tell ";\n"
run (Print e) = do
  tell "printf(\"%d\", "
  runExp e
  tell ");\n"
run (Ret x) = do
  pure x
run (m :>>= f) = do
  x <- run m
  run (f x)
run (Case scrut alts) = do
  s <- run New
  run $ Set s scrut
  let V name = s
  forM_ alts $ \(p, alt) -> do
    runPat name (mkPat p) alt

-- | Generate JS for a compiled pattern.
runPat :: Name -> [Pat] -> Stm () -> RunM ()
runPat scrut (p:ps) alt = do
  case p of
    Match off n -> do
      tell $ concat ["if(v", show scrut, "[", show off, "] == ", show n, "){\n"]
      runPat scrut ps alt
      tell "}\n"
    Bind off name -> do
      tell $ concat ["v", show name, " = v", show scrut, "[", show off, "];\n"]
      runPat scrut ps alt
runPat _ _ alt = do
  run alt

-- | Generate JS for an expression.
runExp :: Exp a -> RunM ()
runExp (Lit l)     = tell $ show l
runExp (Var (V v)) = tell $ concat ["v", show v]

gen :: Stm () -> String
gen = wrap . snd . runWriter . flip evalStateT 0 . run
  where
    wrap s = unlines
      [ "#include <stdio.h>"
      , "int main() {"
      , s
      , "}"
      ]

printProg :: Stm () -> IO ()
printProg = putStrLn . gen

-----

inj :: Typeable p => Exp p -> p
inj = throw . ConstEx

data ConstEx a = ConstEx (Exp a)
instance Show (ConstEx a) where show (ConstEx _) = "ConstEx"
instance Typeable a => Exception (ConstEx a)

adt :: Pattern p => p -> Exp p
adt _ = Var (V 100)

addOff :: Offset -> OffM Offset
addOff off = do
  x <- get
  put (x + off)
  pure x

-- Size of data constructor tags
tagSize = ptrSize

-- Size of pointers
ptrSize = 4

data Pat
  = Match  Offset Int     -- match the hash of a data constructor, or a machine value
  | Bind   Offset Name    -- bind to the given name
    deriving Show

data PatEx = Wildcard | BindVar Name
  deriving Show
instance Exception (PatEx)

data Test a = Foo | Bar a | Barbar a a
  deriving (Generic)

-- | A variable binding.
var :: Var a -> a
var (V v) = throw $ BindVar v

-- | A wildcard.
wc :: a
wc = throw Wildcard

-- | Common functionality for all primitive types.
--   @Int@ is used to represent all machine types.
encodePrim :: Pattern p => (p -> OffM Int) -> p -> OffM [Pat]
encodePrim f x = do
  off <- addOff (sizeOf x)
  ex <- liftIO $ try (flip evalStateT off $ x `seq` f x)
  case ex of
    Left Wildcard    -> pure []
    Left (BindVar v) -> pure [Bind off v]
    Right x'         -> pure [Match off x']

instance Pattern a => Pattern (Test a)
instance Pattern a => Pattern (Maybe a)
instance Pattern Bool where sizeOf _ = 1
instance Pattern Int where
  sizeOf _ = 4
  encodePat = encodePrim pure

encode :: Pattern p => p -> IO [Pat]
encode = flip evalStateT 0 . encodePat

mkPat :: Pattern p => p -> [Pat]
mkPat = unsafePerformIO . encode

class Typeable p => Pattern p where
  sizeOf :: p -> Offset
  sizeOf _ = ptrSize
  
  encodePat :: p -> OffM [Pat]  
  default encodePat :: (Generic p, GPat (Rep p)) => p -> OffM [Pat]
  encodePat = encodePatG . from

  encodeADT :: p -> OffM [Stm ()]
  default encodeADT :: (Generic p, GPat (Rep p)) => p -> OffM [Stm ()]
  encodeADT = adtG . from

class GPat f where
  encodePatG :: f a -> OffM [Pat]
  adtG :: f a -> OffM [Stm ()]

instance GPat U1 where
  encodePatG U1 = pure []
  adtG U1 = pure []

-- Data constructor: a value begins here
instance (GPat a, Constructor c) => GPat (M1 C c a) where
  encodePatG (M1 x) = do
    off <- addOff tagSize
    rest <- encodePatG x
    pure (Match off con : rest)
      where con = hash $ conName (undefined :: M1 C c a ())
  adtG (M1 x) = do
    off <- addOff tagSize
    rest <- adtG x
    pure (SetPtr (Ptr 0) off (Lit con) : rest)
      where con = hash $ conName (undefined :: M1 C c a ())

instance GPat a => GPat (M1 D c a) where
  encodePatG (M1 x) = encodePatG x
  adtG (M1 x) = adtG x

instance GPat a => GPat (M1 S c a) where
  encodePatG (M1 x) = encodePatG x
  adtG (M1 x) = adtG x

-- Value with kind *
instance Pattern a => GPat (K1 i a) where
  encodePatG (K1 x) = do
    off <- addOff (sizeOf x)
    epat <- liftIO $ try (flip evalStateT off $ encodePat x)
    case epat of
      Left Wildcard    -> pure []
      Left (BindVar v) -> pure [Bind off v]
      Right x'         -> pure x'
  adtG (K1 x) = do
    off <- addOff (sizeOf x)
    epat <- liftIO $ try (flip evalStateT off $ encodeADT x)
    case epat of
      Left (ConstEx x') -> pure [SetPtr (Ptr 0) off (x' :: Exp a)]
      Right x'          -> pure x'

instance (GPat a, GPat b) => GPat (a :+: b) where
  encodePatG (L1 x) = encodePatG x
  encodePatG (R1 x) = encodePatG x
  adtG (L1 x) = adtG x
  adtG (R1 x) = adtG x

instance (GPat a, GPat b) => GPat (a :*: b) where
  encodePatG (a :*: b) = do
    left <- encodePatG a
    right <- encodePatG b
    pure (left ++ right)
  adtG (a :*: b) = do
    left <- adtG a
    right <- adtG b
    pure (left ++ right)

with :: (Var a -> Stm b) -> Stm b
with f = do
  x <- New
  f x

-- TODO: actually follow pointers when generating code for this; current ADT
--       view is array of arrays which isn't very useful - otherwise, just use
--       heterogeneous arrays where all elements have size 1 instead of the
--       current mix of the two
-- TODO: how to modify ADTs inside DSL? maybe use record selectors for
--       destructive updates? is that even possible?
-- TODO: inject ADTs into DSL (see @adt@ function); maybe rename Pattern
--       to ADT or something; related to TODO about modifying
-- TODO: make sure vars introduced by @with@ can't be reused
-- TODO: better way to introduce, or wrap introduced, variables;
--       not sure if @with@ is a good idea
-- TODO: encodePrim is very similar to generic encoding - reuse there?
