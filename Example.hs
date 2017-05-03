import Pat hiding (Alg, if_)
import Grape
import CG

readNum :: Stm (Alg (Maybe Int))
readNum = do
  val <- scanN
  if_ (val .== 0)
    (new Nothing)
    (new $ Just (inj val))

-- | Injecting EDSL terms into ADTs and binding values during matching
test :: Stm ()
test = void $ do
  mx <- readNum
  with $ \x -> match' (mx :: Alg (Maybe Int))
    [ Just 1       ~> printS "one"
    , Nothing      ~> printS "nope"
    , Just (var x) ~> printN (val x)
    ]

-- | Injecting EDSL terms into patterns
test2 :: Stm ()
test2 = void $ do
  printS "enter an argument to Right"
  n <- scanN
  x <- new $ (Right 42 :: Either Int Int)
  match' x
    [ Right (inj n) ~> printS "great, you guessed it"
    , Right wc      ~> printS "nope"
    , wc            ~> printS "something is seriously wrong"
    ]

test3 :: Stm ()
test3 = void $ do
  (a, b) <- (,) <$> scanN <*> scanN
  ab <- new (inj a, inj b)
  with $ \x ->
    with $ \y -> match' (ab :: Alg (Int, Int))
      [ (0, wc)        ~> printS "left is 0"
      , (wc, 0)        ~> printS "right is 0"
      , (var x, var y) ~> if_ (val x .== val y) (printS "same") (printS "not same")
      ]

test4 :: Stm ()
test4 = void $ do
  (a, b) <- (,) <$> readNum <*> readNum
  ab <- new (inj a, inj b)
  with $ \x ->
    with $ \y -> match' (ab :: Alg (Alg Int, Alg Int))
      [ (0, wc)        ~> printS "left is 0"
      , (wc, 0)        ~> printS "right is 0"
      , (var x, var y) ~> do
          printS "x"
          with (\z -> match' (val x)
            [ Just (var z) ~> printN (val z)
            , Nothing      ~> printS "nothing"
            ])
          printS "y"
          with (\z -> match' (val y)
            [ Just (var z) ~> printN (val z)
            , Nothing      ~> printS "nothing"
            ])
      ]

main = compileAndRun test
