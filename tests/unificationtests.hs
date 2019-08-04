import Test.HUnit
import Types
import Unification

unificationTest1 =
  TestCase (
    assertEqual
      "?" --TODO
      (Just [])
      (unification [(V (Concrete "x"), V (Concrete "x"))])
  )  
  
unificationTest2 =
  TestCase (
    assertEqual
      "?" --TODO
      Nothing
      (unification [(V (Concrete "x"), V (Concrete "y"))])
  )

unificationTest3 =
  TestCase (
    assertEqual
      "?" --TODO
      (Just [(Meta "X", Concrete "y")])
      (unification [(V (Meta "X"), V (Concrete "y"))])
  )

unificationTest4 =
  TestCase (
    assertEqual
      "?" --TODO
      Nothing
      (unification [(BL [B (Concrete "x") (Meta "X")], BL [])])
  ) 
  
unificationTest5 =
  TestCase (
    assertEqual
      "?" --TODO
      Nothing
      (unification [(BL [], BL [B (Concrete "x") (Meta "X")])])
  )

unificationTest6 =
  TestCase (
    assertEqual
      "?" --TODO
      (Just [(Meta "Y", Concrete "z"), (Meta "X", Concrete "y")])
      (unification [(V (Meta "X"), V (Concrete "y")), (V (Meta "Y"), V (Concrete "z"))])
  )

testsUnification =
  TestList [
    TestLabel "unification_test1" unificationTest1
    , TestLabel "unification_test2" unificationTest2
    , TestLabel "unification_test3" unificationTest3
    , TestLabel "unification_test4" unificationTest4
    , TestLabel "unification_test5" unificationTest5
    , TestLabel "unification_test6" unificationTest6

  ]

main = do
  putStrLn "unification"
  runTestTT testsUnification
  putStrLn ""