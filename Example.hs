import Grape

readNum :: Stm (Exp (Maybe Int))
readNum = do
  val <- scanN
  if_ (val .== 0)
    (new Nothing)
    (new $ Just (inj val))

-- | Injecting EDSL terms into ADTs and binding values during matching
test :: Stm ()
test = void $ do
  mx <- readNum
  with $ \x -> match mx
    [ Just 1       ~> (printS "one" >> pure true)
    , Nothing      ~> (printS "nope" >> pure true)
    , Just (var x) ~> (printN (val x) >> pure true)
    ]

-- | Injecting EDSL terms into patterns
test2 :: Stm ()
test2 = void $ do
  printS "enter an argument to Right"
  n <- scanN
  x <- new $ (Right 42 :: Either Int Int)
  match x
    [ Right (inj n) ~> (printS "great, you guessed it" >> pure true)
    , Right wc      ~> (printS "nope" >> pure true)
    , wc            ~> (printS "something is seriously wrong" >> pure true)
    ]

main = compileAndRun test
