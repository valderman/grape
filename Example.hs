import Trope

readNum :: Stm (Exp (Maybe Int))
readNum = do
  val <- scanN
  if_ (val .== 0)
    (new Nothing)
    (new $ Just (inj val))

test :: Stm ()
test = void $ do
  val <- readNum
  with $ \(v, x) -> match val
    [ Just 1       ~> (printS "one" >> pure true)
    , Nothing      ~> (printS "nope" >> pure true)
    , Just (var v) ~> (printN x >> pure true)
    ]

main = compileAndRun test
