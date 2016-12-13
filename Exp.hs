{-# LANGUAGE GADTs, FlexibleInstances, FlexibleContexts, TypeFamilies #-}
-- | Expression and pattern representations.
module Exp where
import Control.Exception
import Pat hiding (Exp)
import qualified Pat
import Data.Word

-- | Typed variables
newtype Var a = V {varName :: Name}

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

type family StmM :: * -> *

-- | Expression language
data Exp a where
  Prim  :: Prim StmM -> Exp (Prim StmM)
  Const :: Int -> Exp Int
  ToW64 :: Exp a -> Exp Word64
  Word  :: Word64 -> Exp Word64
  Bool  :: Bool -> Exp Bool
  BOp   :: BOp a b -> Exp a -> Exp a -> Exp b
  Var   :: Var a -> Exp a
  Alg   :: Var Ptr -> Exp a
  Undef :: Exp a

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

-- | Inject an EDSL term into an ADT.
inj :: ADT StmM (Exp a) => Exp a -> a
inj = throw . PatEx . asStmM . encAlg

-- | A named wildcard.
var :: ADT StmM a => Var a -> a
var = throw . PatEx . asStmM . Hole . Just . varName

asStmM :: Alg StmM -> Alg StmM
asStmM = id
