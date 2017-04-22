import Pat hiding (if_)
import Grape

readNum :: Stm (ADT Stm (Maybe Int))
readNum = do
  val <- scanN
  if_ (val .== 0)
    (new Nothing)
    (new $ Just (inj val))

-- | Injecting EDSL terms into ADTs and binding values during matching
test :: Stm ()
test = void $ do
  mx <- readNum
  with $ \x -> match' mx
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

main = compileAndRun test
