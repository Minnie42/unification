import Test.HUnit
import Types
import Unification
import Tests.DummyVariables


twoIndependentEquations =
  TestCase (
    assertEqual
      "Two independent equations should be handeled seperately correctly."
      [[(testMetaX, testMetaY)]]
      (unification [(V testMetaX, V testMetaY), (V testConcreteX, V testConcreteX)])
  )

twoDependentEquations =
  TestCase (
    assertEqual
      "Given two dependent equations, the first one should be handeled correctly and the second one\
      \ should be handeled correctly considering the previous changes."
      [[(testMetaY, testConcreteX), (testMetaX, testMetaY)]]
      (unification [(V testMetaX, V testMetaY), (V testMetaX, V testConcreteX)])
  )

cyclicalEquations =
  TestCase (
    assertEqual
      "Given cyclical equations, the first equation should be handeled correctly and the others\
      \ should be handeled correctly considering the previous changes."
      [[(testMetaY, testMetaZ), (testMetaX, testMetaY)]]
      (unification [(V testMetaX, V testMetaY), (V testMetaY, V testMetaZ), (V testMetaZ, V testMetaX)])
  )

multipleEquationsInWhichOneFails =
  TestCase (
    assertEqual
      "Given multiple equations in which one fails, all progress should be dismissed once reaching\
      \ an equation that fails."
      []
      (unification [(V testMetaX, V testMetaY), (V testConcreteX, V testConcreteY)])
  )

testsUnification =
  TestList [
    TestLabel "unification_test1" twoIndependentEquations
    , TestLabel "unification_test2" twoDependentEquations
    , TestLabel "unification_test3" cyclicalEquations
    , TestLabel "unification_test4" multipleEquationsInWhichOneFails
  ]

main = do
  putStrLn "unification"
  runTestTT testsUnification
  putStrLn ""