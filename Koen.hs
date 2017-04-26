{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TypeFamilies, UndecidableInstances #-}
module Koen where

import SAT
import SAT.Bool
import SAT.Equal
import Data.IORef
import Data.Dynamic
import Pat

--------------------------------------------------------------------------------

newtype S a = S ((Solver,[Lit]) -> IO a)

runS :: S a -> IO a
runS (S m) = withNewSolver (\s -> m (s,[]))

instance Applicative S where
  pure x      = S (\s -> pure x)
  S f <*> S x = S (\s -> f s <*> x s)

instance Functor S where
  fmap f (S m) = S (\s -> fmap f (m s))

instance Monad S where
  return    = pure
  S m >>= k = S (\s -> do x <- m s; let S m' = k x in m' s)

lift :: IO a -> S a
lift io = S (\_ -> io)

withSolver :: (Solver -> S a) -> S a
withSolver f = S (\w@(s,_) -> let S m = f s in m w)

withPre :: ([Lit] -> S a) -> S a
withPre f = S (\w@(_,pre) -> let S m = f pre in m w)

when :: [Lit] -> S a -> S a
when xs (S m) = S (\(s,pre) -> m (s,map neg xs ++ pre))

lit :: S Lit
lit = withSolver (lift . newLit)

clause :: [Lit] -> S ()
clause xs = withSolver $ \s -> withPre $ \pre -> lift $ addClause s (pre++xs)

--------------------------------------------------------------------------------

instance If S Lit WORD where
  if_ c mwa mwb =
    do W as <- when [c]     mwa
       W bs <- when [neg c] mwb
       let n      = length as `max` length bs
           pad xs = xs ++ replicate (n - length xs) false
       W `fmap` sequence [ ifb c a b | (a,b) <- pad as `zip` pad bs ]
   where
    ifb c a b =
      do x <- lit
         clause [neg c, neg a, x]
         clause [neg c, a, neg x]
         clause [c, neg b, x]
         clause [c, b, neg x]
         return x

instance If S Lit a => If S WORD a where
  if_ (W cs) a b =
    do c <- withSolver $ \s -> lift $ orl s cs
       if_ c a b

--------------------------------------------------------------------------------

newtype EXPR a = E [WORD] deriving ( Eq, Ord, Show, Typeable )
newtype WORD   = W [Lit]  deriving ( Eq, Ord, Show, Typeable )

instance PatM S where
  type ADT  S = EXPR
  type Prim S = WORD
  type Ref  S = IORef

  alloc ws       = return (E ws)
  index (E ws) i = return (ws !! i)
  slice (E ws) k = return (E (drop k ws))

  fromInt _ n    = W (bin n)
   where
    bin 0 = []
    bin n = bool (odd n) : bin (n `div` 2)

  equals (W xs) (W ys) =
    do b <- withSolver $ \s -> lift $ isEqual s xs ys
       return (W [b])

  setRef _ dyn e =
    case fromDynamic dyn of
      Just ref -> lift $ writeIORef ref e

