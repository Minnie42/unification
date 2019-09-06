import Test.HUnit
import Test.Stats
import Types
import Tests.DummyVariables
import Util
import Data.List

compareBindWithCV = 
  TestCase (
    assertEqual
      "A bind should always be considerd lesser than a chain variable."
      LT
      (testBind `compare` testCV)
  )

compareCVWithBind = 
  TestCase (
    assertEqual
      "A bind should always be considerd lesser than a chain variable."
      GT
      (testCV `compare` testBind)
  )

compareBindWithBindLeftGreater  = 
  TestCase (
    assertEqual
      "A bind with more meta variables should be considered greater."
      GT
      (B testMeta testMeta `compare` B testConcrete testConcrete)
  )

compareBindWithBindLeftLesser  = 
  TestCase (
    assertEqual
      "A bind with more meta variables should be considered greater."
      LT
      (B testConcrete testMeta `compare` B testMeta testMeta)
  )

compareBindWithBindLeftEqual  = 
  TestCase (
    assertEqual
      "Two binds with the same amount of meta variables should be considered equal."
      EQ
      (B testConcrete testMetaX `compare` B testMetaY testConcrete)
  )

compareCVWithCVLeftGreater  = 
  TestCase (
    assertEqual
      "A chain variable with more meta variables should be considered greater."
      GT
      (CV "A" testMeta testMeta `compare` CV "B" testMeta testConcrete)
  )

compareCVWithCVLeftLesser  = 
  TestCase (
    assertEqual
      "A chain variable with less meta variables should be considered lesser."
      LT
      (CV "A" testConcrete testConcrete `compare` CV "B" testMeta testConcrete)
  )

compareCVWithCVLeftEqual  = 
  TestCase (
    assertEqual
      "Two chain variable with the same amount of meta variables should be considered equal."
      EQ
      (CV "A" testConcrete testMetaX `compare` CV "B" testMetaY testConcrete)
  )

compareTwoEquationsOfLeastClass =
  TestCase (
    assertEqual
      "Two equations that contain an empty set should be considered equal."
      EQ
      ((BL [], BL []) `compareEquations` (BL [], BL [testBind]))
  )

compareEquationOfVariablesWithEquationWIthEmptySet =
  TestCase (
    assertEqual
      "An equation that contains an empty set should be considered lesser than an equation with two variables."
      LT
      ((BL [], BL [testBind]) `compareEquations` (V testVar, V testVar))
  )

compareTwoEquationsOfVariables =
  TestCase (
    assertEqual
      "Two equations that contain only variables should be considered equal."
      EQ
      ((V testMeta, V testMeta) `compareEquations` (V testConcrete, V testConcrete))
  )

compareTwoEquationsOfBindsOneWIthEmptySet =
  TestCase (
    assertEqual
      "An equations of binds that contains an empty set should be considered less than an equation of binds without an empty set."
      LT
      ((BL [], BL [testBind]) `compareEquations` (BL [testBind], BL [testBind]))
  )

compareTwoEquationsOneOfBindsOneOfVariables =
  TestCase (
    assertEqual
      "An equations of variables should be considered less than an equation of binds."
      LT
      ((V testVar, V testVar) `compareEquations` (BL [testBind], BL [testBind]))
  )

compareTwoEquationsOfBindsOfDiffentSize =
  TestCase (
    assertEqual
      "An equations of greater size should be considered greater."
      GT
      ((BL [testBind, testBind], BL [testBind, testBind])`compareEquations` (BL [testBind], BL [testBind, testBind]))
  ) 

compareTwoEquationsOfSameSizeOneWithChainVariables =
  TestCase (
    assertEqual
      "An equation of same size but with chain variables should be considered greater than an equation without chain variables."
      LT
      ((BL [testBind, testBind], BL [testBind, testBind])`compareEquations` (BL [CV "test1" testVar testVar, testBind], BL [CV "test2" testVar testVar, testBind]))
  )  

compareTwoEquationsOfSameSizeBothWithChainVariables =
  TestCase (
    assertEqual
      "Two equations of same size with same amount of chain variables should be considered equal."
      EQ
      ((BL [CV "test3" testVar testVar, CV "test4" testVar testVar], BL [testBind, testBind])`compareEquations` (BL [CV "test1" testVar testVar, testBind], BL [CV "test2" testVar testVar, testBind]))
  ) 

compareTwoEquationsOfDiffentSizeSmallerOneWithChainVariables =
  TestCase (
    assertEqual
      "An equation with chain variables should be considered greater than an equation without chain variables."
      LT
      ((BL [testBind, testBind, testBind], BL [testBind, testBind])`compareEquations` (BL [CV "test1" testVar testVar, testBind], BL [CV "test2" testVar testVar, testBind]))
  )  

sortThreeEquations =
  TestCase (
    assertEqual
      "Equations should be sorted correctly."
      [(BL [], BL [testBind]), (V testVarX, V testVarY), (BL [CV "test" testVarX testVarY], BL [testBind])]
      (sortBy compareEquations [(BL [CV "test" testVarX testVarY], BL [testBind]), (BL [], BL [testBind]), (V testVarX, V testVarY)])
  ) 

-------------------------------
-- Tests ----------------------
-------------------------------
testsBind = 
  TestList [
    TestLabel "bind_test1" compareBindWithCV
    , TestLabel "bind_test2" compareCVWithBind
    , TestLabel "bind_test3" compareBindWithBindLeftGreater
    , TestLabel "bind_test4" compareBindWithBindLeftLesser
    , TestLabel "bind_test5" compareBindWithBindLeftEqual
    , TestLabel "bind_test6" compareCVWithCVLeftGreater
    , TestLabel "bind_test7" compareCVWithCVLeftLesser
    , TestLabel "bind_test8" compareCVWithCVLeftEqual
  ]

testsEquation = 
  TestList [
    TestLabel "equation_test1" compareTwoEquationsOfLeastClass
    , TestLabel "equation_test2" compareEquationOfVariablesWithEquationWIthEmptySet
    , TestLabel "equation_test3" compareTwoEquationsOfVariables
    , TestLabel "equation_test4" compareTwoEquationsOfBindsOneWIthEmptySet
    , TestLabel "equation_test5" compareTwoEquationsOneOfBindsOneOfVariables
    , TestLabel "equation_test6" compareTwoEquationsOfBindsOfDiffentSize
    , TestLabel "equation_test7" compareTwoEquationsOfSameSizeOneWithChainVariables
    , TestLabel "equation_test8" compareTwoEquationsOfSameSizeBothWithChainVariables
    , TestLabel "equation_test9" compareTwoEquationsOfDiffentSizeSmallerOneWithChainVariables
    , TestLabel "equation_test10" sortThreeEquations
  ]

main = combineTests [
    (testsBind, "bind")
    , (testsEquation, "equation")
  ]