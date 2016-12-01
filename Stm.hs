{-# LANGUAGE GADTs #-}
-- | Statement language: side effects and conditionals
--   TODO: abstract out a monad in which an ADT can be matched/serialized
--   into a type class
module Stm where
import Exp
import Pat
import Control.Monad

type Case a b = (Pat a, Stm (Exp b))
(~>) :: ADT a => a -> Stm (Exp b) -> Case a b
a ~> b = (pat a, b)
infixr 9 ~>

data Stm a where
  -- Monad ops
  Return :: a -> Stm a
  Bind   :: Stm a -> (a -> Stm b) -> Stm b

  -- I/O
  Print  :: String -> Stm ()
  PrintN :: Exp Int -> Stm ()
  Scan   :: Stm (Exp Int)

  -- Refs
  Set    :: Var a -> Exp a -> Stm ()
  Get    :: Var a -> Stm (Exp a)
  NewRef :: Exp a -> Stm (Var a)

  -- Loading/storing things; offsets are in # of machine words
  Read   :: Exp Ptr -> Int -> Stm (Exp a)
  Write  :: Exp Ptr -> Int -> Exp a -> Stm ()
  Alloca :: Int -> Stm (Exp Ptr)

  -- Conditionals
  If     :: Exp Bool -> Stm (Exp a) -> Stm (Exp a) -> Stm (Exp a)

  -- Untimely termination
  Die    :: Stm (Exp a)

instance Functor Stm where
  fmap f m = m >>= return . f

instance Applicative Stm where
  (<*>) = ap
  pure  = return

instance Monad Stm where
  return = Return
  (>>=)  = Bind

-- | How much memory is needed by the constructor and pointers to arguments?
allocSize :: Alg -> Int
allocSize (Prim _)   = 1
allocSize (Con _ as) = length as + 1
allocSize (Hole _)   = error "holes have no size, silly"

new :: ADT a => a -> Stm (Exp a)
new x = do
    v <- NewRef =<< store' (encAlg x)
    return (Alg v)
  where
    store' alg = do
      ptr <- Alloca $ allocSize alg
      go ptr alg
      return ptr
    go ptr (Prim (PInt n)) = Write ptr 0 n
    go ptr (Prim (PBool b)) = Write ptr 0 b
    go ptr (Con t as) = do
      Write ptr 0 (Const t)
      ptrs <- mapM store' as
      zipWithM_ (Write ptr) [1..] ptrs
    go _ (Hole _) = do
      error "can't store holes, silly"

match :: ADT a => Exp a -> [Case a b] -> Stm (Exp b)
match pp@(Alg v) ((Pat p, s):cs) = do
  matches <- matchOne (Var v) p 0
  If matches s (match pp cs)
match (Var (V v)) ps = do
  match (Alg (V v)) ps
match _ _ = do
  Print "incomplete pattern match"
  Die

matchOne :: Exp Ptr -> Alg -> Int -> Stm (Exp Bool)
matchOne ptr pat off = do
  case pat of
    Prim (PBool b) -> do
      (.== b) <$> Read ptr off
    Prim (PInt n) -> do
      (.== n) <$> Read ptr off
    Hole Nothing -> do
      pure (Bool True)
    Hole (Just n) -> do
      Read ptr off >>= Set (V n) >> pure (Bool True)
    Con t as -> do
      t' <- Read ptr off
      flip (If (Const t .== t')) (pure (Bool False)) $ do
        ress <- forM (zip as [off+1 ..]) $ \(pat', off') -> do
          ptr' <- Read ptr off'
          matchOne ptr' pat' 0
        pure $ foldr (.&&) (Bool True) ress
