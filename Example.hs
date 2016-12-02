import Grape

readNum :: Stm (Exp (Maybe Int))
readNum = do
  val <- scanN
  if_ (val .== 0)
    (new Nothing)
    (new $ Just (inj val))

test :: Stm ()
test = void $ do
  mx <- readNum
  with $ \x -> match mx
    [ Just 1       ~> (printS "one" >> pure true)
    , Nothing      ~> (printS "nope" >> pure true)
    , Just (var x) ~> (printN (val x) >> pure true)
    ]

main = compileAndRun test
