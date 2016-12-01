{-# LANGUAGE GADTs, FlexibleInstances, FlexibleContexts, TypeFamilies #-}
-- | Expression and pattern representations.
module Exp where
import Control.Exception
import Pat

-- | Typed variables
newtype Var a = V {varName :: Name}

-- | Pointers: necessary to store ADTs
type Ptr = Int

-- | Binary operators
data BOp a b where
  Add :: BOp Int  Int
  Sub :: BOp Int  Int
  Mul :: BOp Int  Int
  Div :: BOp Int  Int
  Mod :: BOp Int  Int
  Eq  :: BOp a    Bool
  Neq :: BOp a    Bool
  Gt  :: BOp a    Bool
  Lt  :: BOp a    Bool
  Ge  :: BOp a    Bool
  Le  :: BOp a    Bool
  And :: BOp Bool Bool
  Or  :: BOp Bool Bool

-- | Expression language
data Exp a where
  Const :: Int -> Exp Int
  Bool  :: Bool -> Exp Bool
  BOp   :: BOp a b -> Exp a -> Exp a -> Exp b
  Var   :: Var a -> Exp a
  Alg   :: Var Ptr -> Exp a
  Undef :: Exp a

-- | Primitive type for ADT encoding
data instance PrimType where
  PInt  :: Exp Int -> PrimType
  PBool :: Exp Bool -> PrimType

instance Show (Exp Int) where
  show (Const n)   = show n
  show (Var (V n)) = 'v' : show n

instance Num (Exp Int) where
  fromInteger = Const . fromInteger
  (+)         = BOp Add
  (-)         = BOp Sub
  (*)         = BOp Mul

undef :: Exp a
undef = Undef

true, false :: Exp Bool
[true, false] = map Bool [True, False]

not_ :: Exp Bool -> Exp Bool
not_ = (.== false)

(./), (.%) :: Exp Int -> Exp Int -> Exp Int
[(./), (.%)] = map BOp [Div, Mod]

(.&&), (.||) :: Exp Bool -> Exp Bool -> Exp Bool
[(.&&), (.||)] = map BOp [And, Or]

(.>), (.<), (.>=), (.<=), (.==), (!=) :: Exp a -> Exp a -> Exp Bool
[(.>), (.<), (.>=), (.<=), (.==), (!=)] = map BOp [Gt, Lt, Ge, Le, Eq, Neq]

-- | Pattern representation.
newtype Pat a = Pat {unPat :: Alg}

-- | Build a pattern from an ADT.
pat :: ADT a => a -> Pat a
pat = Pat . encAlg

-- | Inject an EDSL term into an ADT.
inj :: ADT (Exp a) => Exp a -> a
inj = throw . PatEx . encAlg

-- | An unnamed wildcard.
wc :: ADT a => a
wc = throw $ PatEx $ Hole Nothing

-- | A named wildcard.
var :: ADT a => Var a -> a
var = throw . PatEx . Hole . Just . varName
