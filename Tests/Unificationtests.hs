import Test.HUnit
import Test.Stats
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

emptyProblem =
  TestCase (
    assertEqual
      "An empty problem should return an empty solution."
      [[]]
      (unification [])
  )

twoBrachchesBothValid =
  TestCase (
    assertEqual
      "A problem with two valid solutions should return both."
      [[(testMetaY, testMetaZ), (testMetaX, testMetaZ)], [(testMetaX, testMetaZ), (testMetaY, testMetaX)]]
      (unification [(BL [B testMetaX testMetaY, B testMetaY testMetaZ], BL [B testMetaX testMetaX, B testMetaZ testMetaX])])
  )

twoBrachchesOneInvalid =
  TestCase (
    assertEqual
      "This test covers the removing of invaled branches."
      [[(testMetaX, testConcreteY), (testMetaY, testMetaX), (testMetaZ, testConcreteX)]]
      (unification [(BL [B testConcreteX testMetaY, B testMetaY testMetaZ], BL [B testConcreteY testMetaZ, B testMetaZ testMetaX])])
  )

chainVariableTwoValidBranches =
  TestCase (
    assertEqual
      "After the expansion of the chain variable all valid branches should be calculated correctly."
      [
        [(testMetaZ, testConcreteX), (testMetaY, testConcreteX), ((Meta "CVtest1"), testMetaY), (testMetaX, testMetaZ)]
      , [((Meta "CVtest1"), testMetaZ), (testMetaX, testConcreteX)]
      ]
      (unification [(BL [CV "test" testMetaX testMetaY], BL [B testConcreteX testMetaZ, B testMetaZ testMetaY])])
  )

chainVariableOneInvalidBranches =
  TestCase (
    assertEqual
      "After the expansion of the chain variable all invalid branches should be removed correctly."
      [
        [(testMetaY, testConcreteY), (testMetaZ, testConcreteX), (Meta "CVtest1", testMetaZ), (testMetaX, testConcreteX)]
      ]
      (unification [(BL [CV "test" testMetaX testMetaY], BL [B testConcreteX testMetaZ, B testMetaX testConcreteY])])
  )

testsUnification =
  TestList [
    TestLabel "unification_test1" twoIndependentEquations
    , TestLabel "unification_test2" twoDependentEquations
    , TestLabel "unification_test3" cyclicalEquations
    , TestLabel "unification_test4" multipleEquationsInWhichOneFails
    , TestLabel "unification_test5" emptyProblem
    , TestLabel "unification_test6" twoBrachchesBothValid
    , TestLabel "unification_test7" twoBrachchesOneInvalid
    , TestLabel "unification_test8" chainVariableTwoValidBranches
    , TestLabel "unification_test9" chainVariableOneInvalidBranches
  ]

main = combineTests [
  (testsUnification, "unification")
  ]