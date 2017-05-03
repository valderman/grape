{-# LANGUAGE GADTs, FlexibleInstances #-}
-- | Expression and pattern representations.
module Exp where
import Pat (ADT)

-- | Typed variables
newtype Var a = V {varName :: Int}

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
  B2I   :: Exp Bool -> Exp Int
  I2B   :: Exp Int -> Exp Bool
  BOp   :: BOp a b -> Exp a -> Exp a -> Exp b
  Var   :: Var a -> Exp a
  Undef :: Exp a

unsafeFreeze :: Var a -> Exp a
unsafeFreeze = Var

instance Show (Exp Int) where
  show (Const n)   = show n
  show (Var (V n)) = 'v' : show n

instance Num (Exp Int) where
  fromInteger = Const . fromInteger
  (+)         = BOp Add
  (-)         = BOp Sub
  (*)         = BOp Mul

i2b :: Exp Int -> Exp Bool
i2b = I2B

b2i :: Exp Bool -> Exp Int
b2i = B2I

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
