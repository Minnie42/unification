import Test.HUnit
import Test.Stats
import Types
import Util
import Unification
import Tests.DummyVariables


twoIndependentEquations =
  TestCase (
    assertEqual
      "Two independent equations should be handeled seperately correctly."
      [[(Sub testMetaX testMetaY)]]
      (unification [(V testMetaX, V testMetaY), (V testConcreteX, V testConcreteX)])
  )

twoDependentEquations =
  TestCase (
    assertEqual
      "Given two dependent equations, the first one should be handeled correctly and the second one\
      \ should be handeled correctly considering the previous changes."
      [[(Sub testMetaY testConcreteX), (Sub testMetaX testMetaY)]]
      (unification [(V testMetaX, V testMetaY), (V testMetaX, V testConcreteX)])
  )

cyclicalEquations =
  TestCase (
    assertEqual
      "Given cyclical equations, the first equation should be handeled correctly and the others\
      \ should be handeled correctly considering the previous changes."
      [[(Sub testMetaY testMetaZ), (Sub testMetaX testMetaY)]]
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
      [[(Sub testMetaY testMetaZ), (Sub testMetaX testMetaZ)], [(Sub testMetaX testMetaZ), (Sub testMetaY testMetaX)]]
      (unification [(BL [B testMetaX testMetaY, B testMetaY testMetaZ], BL [B testMetaX testMetaX, B testMetaZ testMetaX])])
  )

twoBrachchesOneInvalid =
  TestCase (
    assertEqual
      "This test covers the removing of invaled branches."
      [[(Sub testMetaX testConcreteY), (Sub testMetaY testMetaX), (Sub testMetaZ testConcreteX)]]
      (unification [(BL [B testConcreteX testMetaY, B testMetaY testMetaZ], BL [B testConcreteY testMetaZ, B testMetaZ testMetaX])])
  )

chainVariableTwoValidBranches =
  TestCase (
    assertEqual
      "After the expansion of the chain variable all valid branches should be calculated correctly."
      [
        [(Sub testMetaZ testConcreteX), (Sub testMetaY testConcreteX), (Sub (Meta "CVtest1") testMetaY), (Sub testMetaX testMetaZ), (Exp "test" 2)]
      , [(Sub (Meta "CVtest1") testMetaZ), (Sub testMetaX testConcreteX), (Exp "test" 2)]
      ]
      (unification [(BL [CV "test" testMetaX testMetaY], BL [B testConcreteX testMetaZ, B testMetaZ testMetaY])])
  )

chainVariableOneInvalidBranches =
  TestCase (
    assertEqual
      "After the expansion of the chain variable all invalid branches should be removed correctly."
      [
        [(Sub testMetaY testConcreteY), (Sub testMetaZ testConcreteX), (Sub (Meta "CVtest1") testMetaZ), (Sub testMetaX testConcreteX), (Exp "test" 2)]
      ]
      (unification [(BL [CV "test" testMetaX testMetaY], BL [B testConcreteX testMetaZ, B testMetaX testConcreteY])])
  )

validSolutionsForChainVariables =
  TestCase (
    assertEqual
      "All calculated solutions should solve the problem."
      True
      (foldl (&&) True (map (\sol -> isProblemSolved (applySolutionToGamma sol problem)) (unification problem)))
  )
  where
    problem = [(BL [CV "S" testMetaX testMetaY, CV "T" testMetaW testMetaZ, CV "U" testMetaA testMetaB, CV "V" testMetaW testMetaY, CV "V" testMetaW testMetaZ, CV "V" testMetaX testMetaY], BL [CV "Y" testMetaX testMetaW, CV "Z" testMetaA testMetaB])]

twoIdenticalChainVariablesWithAditionalBindOnOneSide =
  TestCase (
    assertEqual
      "Two identical chain variables on each side of the equation with an different amount of additional bindings on each side should resolve to no solution."
      []
      (unification [(BL [CV "S" testMetaX testMetaY, B testVar testVar], BL [CV "S" testMetaX testMetaY])])
  )

twoIdenticalChainVariablesWithConcreteVariables =
  TestCase (
    assertEqual
      "Two identical chain variables with concrete variables on each side should resolve to 6 solutions."
      6
      (length (unification [(BL [CV "S" testConcreteX testConcreteY], BL [CV "S" testConcreteX testConcreteY])]))
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
    , TestLabel "unification_test10" validSolutionsForChainVariables
    , TestLabel "unification_test11" twoIdenticalChainVariablesWithAditionalBindOnOneSide
    , TestLabel "unification_test12" twoIdenticalChainVariablesWithConcreteVariables
  ]

main = combineTests [
  (testsUnification, "unification")
  ]