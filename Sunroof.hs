{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables, InstanceSigs, DataKinds, FlexibleContexts #-}
module Sunroof where
import Control.Monad
import Language.Sunroof as S
import Data.Boolean
import Data.Dynamic
import Pat

-- | Underlying primitive JS type
type JST = JSArray JSString

-- | Algebraic JS type
newtype JSAlg a = JSAlg JST

-- | Reference to algebraic JS type
newtype JSAlgRef a = JSAlgRef (JSRef JST)

instance SunroofThread t => If (JS t) JSString JSString where
  if_ c = ifB (S.cast c :: JSBool)

instance (SunroofThread t, Typeable t) => PatM (JS t) where
  type Prim (JS t) = JSString
  type Ref (JS t) = JSAlgRef
  type ADT (JS t) = JSAlg

  alloc xs = do
    arr <- array ([] :: [String])
    mapM_ (flip push arr) xs
    return $ JSAlg arr

  index (JSAlg arr) i = pure $ arr ! S.index (fromIntegral i)

  -- Sunroof doesn't have slice, so implement it by copying the array and then
  -- unshifting one element for each offset index.
  slice (JSAlg arr) off = do
    arr' <- array ([] :: [String])
    forEach (flip push arr') arr
    sequence_ [shift arr' | _ <- [1..off]]
    return $ JSAlg arr'

  equals a b = pure $ S.cast $ a ==* b

  fromInt _ = S.cast . (fromIntegral :: Int -> JSNumber)

  setRef :: forall a. Typeable a => Bool -> Dynamic -> ADT (JS t) a -> JS t ()
  setRef _ ref (JSAlg x) =
    case fromDynamic ref of
      Just (JSAlgRef ref' :: JSAlgRef a) -> writeJSRef x ref'

type instance Term (JS t) Bool = JSBool

instance (SunroofThread t, Typeable t) => Algebraic (JS t) JSBool where
  encode = encodePrim . S.cast

-- | An unnamed, blocking wildcard.
wcb :: Algebraic (JS B) a => a
wcb = wcFor (Proxy :: Proxy (JS B))

-- | Inject an EDSL term into an ADT.
inj :: Sunroof a => Algebraic (JS B) (Term (JS B) a) => Term (JS B) a -> a
inj = injFor (Proxy :: Proxy (JS B))

-- | A named wildcard.
var :: forall a. (Typeable a, Algebraic (JS B) a) => JSAlgRef a -> a
var = varFor (Proxy :: Proxy (JS B)) isPrim
  where
    isPrim = typeRep (Proxy :: Proxy a) == typeRep (Proxy :: Proxy Int)
