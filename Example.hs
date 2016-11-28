import Patterns

test :: Stm ()
test = do
  v <- New
  Set v 27
  with $ \x -> Case (adt $ Just (Barbar (inj (Var v)) 30))
    [ (Just (Barbar wc (var x)), Print (unsafeFreeze x))
      -- this should be an error, since @x@ is reused:
--    , (Nothing,                  Case undefined [(Just (var x), Print (unsafeFreeze x))])
    , (Nothing,                  Print (Lit 100))
    ]
  Print (unsafeFreeze v)
