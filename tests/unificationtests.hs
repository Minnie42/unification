import Test.HUnit
import Types
import Unification

unificationTest1 =
  TestCase (
    assertEqual
      "two identical concretes should be removed"
      [[]]
      (unification [(V (Concrete "x"), V (Concrete "x"))])
  )  
 
unificationTest2 =
  TestCase (
    assertEqual
      "an equation with two different concretes should resolve to Nothing"
      []
      (unification [(V (Concrete "x"), V (Concrete "y"))])
  )
 
unificationTest3 =
  TestCase (
    assertEqual
      "an equation with one meta and one concrete variable should resolve to a map? with these two" --TODO
      [[(Meta "X", Concrete "y")]]
      (unification [(V (Meta "X"), V (Concrete "y"))])
  )

unificationTest4 =
  TestCase (
    assertEqual
      "to binds one empty should resolve to Nothing"
      []
      (unification [(BL [B (Concrete "x") (Meta "X")], BL [])])
  ) 
  
unificationTest5 =
  TestCase (
    assertEqual
      "to binds one empty should resolve to Nothing"
      []
      (unification [(BL [], BL [B (Concrete "x") (Meta "X")])])
  )

unificationTest6 =
  TestCase (
    assertEqual
      "two equations with each a meta and a concrete variable should resolve to a list of tupel with these ?" --TODO
      [[(Meta "Y", Concrete "z"), (Meta "X", Concrete "y")]]
      (unification [(V (Meta "X"), V (Concrete "y")), (V (Meta "Y"), V (Concrete "z"))])
  )

unificationTest7 =
  TestCase (
    assertEqual
      "zyklisch"--TODO
      []
      (unification [(V (Meta"X"), V (Meta"Y")), (V (Meta"Y"), V (Meta"Z")), (V (Meta"Z"), V (Meta"X"))])
  )

testsUnification =
  TestList [
    TestLabel "unification_test1" unificationTest1
    , TestLabel "unification_test2" unificationTest2
    , TestLabel "unification_test3" unificationTest3
    , TestLabel "unification_test4" unificationTest4
    , TestLabel "unification_test5" unificationTest5
    , TestLabel "unification_test6" unificationTest6
    , TestLabel "unification_test7" unificationTest7

  ]

main = do
  putStrLn "unification"
  runTestTT testsUnification
  putStrLn ""