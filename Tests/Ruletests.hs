import Test.HUnit
import Types
import Rules
import Util
import Tests.DummyVariables

-------------------------------
-- Rule 1 ---------------------
-------------------------------
rule1EquationEachSideBindOfSizeOne =
  TestCase (
    assertEqual
      "An equation with a bind of size one on each side should combine the first variable\
      \ of the left bind with the first variable of the right bind and the second variable of\
      \ the right bind with the second variable of the left bind."
      (Just 
        (
          testSol
        , (V testVarX, V testVarX):(V testVarY, V testVarY):testGamma
        ) 
      )
      (
        rule1 
          testSol 
          ((BL [B testVarX testVarY], BL [B testVarX testVarY]):testGamma)
        )
  )

rule1EquationWithBindOfSizeTwoOnRightSide =
  TestCase (
    assertEqual
      "An equation with a bind of size greater then one on the right side should resolve to nothing."
      Nothing
      (
        rule1 
          testSol 
          ((BL [testBind], BL [testBind, testBind]):testGamma)
      )
  )

rule1EquationWithBindOfSizeZeroOnTheRightSide =
  TestCase (
    assertEqual
      "An equation with a bind of size zero on the right side should resolve to nothing."
      Nothing
      (
        rule1 
          testSol 
          ((BL [testBind], BL []):testGamma)
      )
  )
  
rule1EquationWithBindOfSizeTwoOnLeftSide =
  TestCase (
    assertEqual
      "An equation with a bind of size greater then one on the left side should resolve to nothing."
      Nothing
      (
        rule1 
          testSol 
          ((BL [testBind, testBind], BL [testBind]):testGamma)
      )
  )

rule1EquationWithBindOfSizeZeroOnTheLeftSide =
  TestCase (
    assertEqual
      "An equation with a bind of size zero on the left side should resolve to nothing."
      Nothing
      (
        rule1 
          testSol 
          ((BL [], BL [testBind]):testGamma)
      )
  )  
  
rule1EquationWithVarOnEachSide =
  TestCase (
    assertEqual
      "An equation with a variable on each side should resolve to nothing."
      Nothing
      (
        rule1 
          testSol 
          ((V testVar, V testVar):testGamma)
      )
  )    

-------------------------------
-- Rule 2 ---------------------
-------------------------------
rule2EquationWithTwoIdenticalConcretes =
  TestCase (
    assertEqual
      "An equation with two identical concretes should be removed."
      (Just (testSol, testGamma))
      (
        rule2 
          testSol 
          ((V testConcreteX, V testConcreteX):testGamma)
      )
  )

rule2EquationWithTwoIdenticalMetas =
  TestCase (
    assertEqual
      "An equation with two identical metas should be removed."
      (Just (testSol, testGamma))
      (
        rule2 
          testSol 
          ((V testMetaX, V testMetaX):testGamma)
      )
  )

rule2EquationWithTwoDifferentConcretes =
  TestCase (
    assertEqual
      "An equation with two different concretes should resolve to nothing.."
      Nothing
      (
        rule2 
          testSol 
          ((V testConcreteX, V testConcreteY):testGamma)
      )
  )

rule2EquationWithTwoDifferentMetas =
  TestCase (
    assertEqual
      "An equation with two different metas should be removed."
      Nothing
      (
        rule2 
          testSol 
          ((V testMetaX, V testMetaY):testGamma)
      )
  )

rule2EquationWithMetaOnTheLeftAndConcreteOnTheRight =
  TestCase (
    assertEqual
      "An equation with a meta variable on the left and a concrete variable on the right should resolve to nothing."
      Nothing
      (
        rule2 
          testSol 
          ((V testMeta, V testConcrete):testGamma)
      )
  )

rule2EquationWithConcreteOnTheLeftAndMetaOnTheRight =
  TestCase (
    assertEqual
      "An equation with a concrete variable on the left and a meta variable on the right should resolve to nothing."
      Nothing
      (
        rule2 
          testSol 
          ((V testConcrete, V testMeta):testGamma)
      )
  )

rule2EquationWithTwoBinds =
  TestCase (
    assertEqual
      "An equation with two binds should resolve to nothing."
      Nothing
      (
        rule2 
          testSol 
          ((BL [testBind], BL [testBind]):testGamma)
      )
  )

-------------------------------
-- Rule 3 ---------------------
-------------------------------
rule3EquationWithTwoDifferentConcretes =
  TestCase (
    assertEqual
      "An equation with two different concrete variables should transform the current solution to nothing."
      (
        Just (
          Nothing
        , ((V testConcreteX, V testConcreteY):testGamma)
        )
      )
      (
        rule3 
          testSol 
          ((V testConcreteX, V testConcreteY):testGamma)
      )
  )
  
rule3EquationWithTwoIdenticalConcretes =
  TestCase (
    assertEqual
      "An equation with two identical concrete variables should resolve to nothing."
      Nothing
      (
        rule3 
          testSol 
          ((V testConcreteX, V testConcreteX):testGamma)
      )
  )
  
rule3EquationWithTwoIdenticalMetas =
  TestCase (
    assertEqual
      "An equation with two identical meta variables should resolve to nothing."
      Nothing
      (
        rule3 
          testSol 
          ((V testMetaX, V testMetaX):testGamma)
      )
  )

rule3EquationWithTwoDifferentMetas =
  TestCase (
    assertEqual
      "An equation with two different meta variables should resolve to nothing."
      Nothing
      (
        rule3 
          testSol 
          ((V testMetaX, V testMetaY):testGamma)
      )
  )

rule3EquationWithTwoBinds =
  TestCase (
    assertEqual
      "An equation with two binds should resolve to nothing."
      Nothing
      (
        rule3 
          testSol 
          ((BL [testBind], BL [testBind]):testGamma)
      )
  )

rule3EquationWithConcreteOnTheLeftAndMetaOnTheRight =
  TestCase (
    assertEqual
      "An equation with a concrete variable on the left and a meta variable on the right should resolve to nothing."
      Nothing
      (
        rule3 
          testSol 
          ((V testConcrete, V testMeta):testGamma)
      )
  )

rule3EquationWithMetaOnTheLeftAndConcreteOnTheRight =
  TestCase (
    assertEqual
      "An equation with a meta variable on the left and a concrete variable on the right should resolve to nothing."
      Nothing
      (
        rule3 
          testSol 
          ((V testMeta, V testConcrete):testGamma)
      )
  )
-------------------------------
-- Rule 4 ---------------------
-------------------------------
rule4EquationWithMetaOnTheLeftAndConcreteOnTheRight =
  TestCase (
    assertEqual
      "An equation with one meta variable on the left side and one concrete variable on the right should apply a\
      \ substitution from the meta variable to the concrete variable."
      (
        Just (
          (Just ((testMetaX, testConcreteX):testSol))
        , ((V testConcreteX, V testConcreteX):(applySubstitutionToGamma (testMetaX, testConcreteX) testGamma))
        )
      )
      (
        rule4 
          (Just testSol) 
          ((V testMetaX, V testConcreteX):testGamma)
      )
  )
  where
    testSol = []

rule4EquationWithConcreteOnTheLeftAndMetaOnTheRight =
  TestCase (
    assertEqual
      "An equation with one concrete variable on the left side and one meta variable on the right should apply a\
      \ substitution from the meta variable to the concrete variable."
      (
        Just (
          (Just ((testMetaX, testConcreteX):testSol))
        , ((V testConcreteX, V testConcreteX):(applySubstitutionToGamma (testMetaX, testConcreteX) testGamma))
        )
      )
      (
        rule4 
          (Just testSol) 
          ((V testConcreteX, V testMetaX):testGamma)
      )
  )
  where
    testSol = []


rule4EquationWithTwoMetas =
  TestCase (
    assertEqual
      "An equation with two meta variables should resolve to nothing."
      Nothing
      (
        rule4 
          testSol 
          ((V testMeta, V testMeta):testGamma)
      )
  )

rule4EquationWithTwoConcretes =
  TestCase (
    assertEqual
      "An equation with two concrete variables should resolve to nothing."
      Nothing
      (
        rule4 
          testSol 
          ((V testConcrete, V testConcrete):testGamma)
      )
  )

rule4EquationWithTwoBinds =
  TestCase (
    assertEqual
      "An equation with two binds should resolve to nothing."
      Nothing
      (
        rule4 
          testSol 
          ((BL [testBind], BL [testBind]):testGamma)
      )
  )
-------------------------------
-- Rule 5 ---------------------
-------------------------------
rule5EquationWithTwoDiffentMetas =
  TestCase (
    assertEqual
      "An equation with two different meta variables should apply a substitution from the meta\
      \ variable on the left of the equation to the meta variable on the right."
      (
        Just (
          (Just ((testMetaX, testMetaY):testSol))
        , ((V testMetaY, V testMetaY):(applySubstitutionToGamma (testMetaX, testMetaY) testGamma))
        )
      )
      (
        rule5 
          (Just testSol) 
          ((V testMetaX, V testMetaY):testGamma)
      )
  )
  where
    testSol = []

rule5EquationWithTwoIdenticalMetas =
  TestCase (
    assertEqual
      "An equation with two identical meta variables should resolve to nothing."
      Nothing
      (
        rule5 
          testSol 
          ((V testMetaX, V testMetaX):testGamma)
      )
  )
  
rule5EquationWithTwoIdenticalConcretes =
  TestCase (
    assertEqual
      "An equation with two identical concrete variables should resolve to nothing."
      Nothing
      (
        rule5 
          testSol 
          ((V testConcreteX, V testConcreteX):testGamma)
      )
  )
  
rule5EquationWithTwoDifferentConcretes =
  TestCase (
    assertEqual
      "An equation with two different concrete variables should resolve to nothing."
      Nothing
      (
        rule5 
          testSol 
          ((V testConcreteX, V testConcreteY):testGamma)
      )
  )

rule5EquationWithTwoBinds =
  TestCase (
    assertEqual
      "An equation with two binds should resolve to nothing."
      Nothing
      (
        rule5 
          testSol 
          ((BL [testBind], BL [testBind]):testGamma)
      )
  )

rule5EquationWithConcreteOnTheLeftAndMetaOnTheRight =
  TestCase (
    assertEqual
      "An equation with a concrete variable on the left and a meta variable on the right should resolve to nothing."
      Nothing
      (
        rule5 
          testSol 
          ((V testConcrete, V testMeta):testGamma)
      )
  )

rule5EquationWithMetaOnTheLeftAndConcreteOnTheRight =
  TestCase (
    assertEqual
      "An equation with a meta variable on the left and a concrete variable on the right should resolve to nothing."
      Nothing
      (
        rule5
          testSol 
          ((V testMeta, V testConcrete):testGamma)
      )
  )
-------------------------------
-- Rule 6 ---------------------
-------------------------------
rule6EquationWithBindsGreaterThenOneOnBothSides =
  TestCase (
    assertEqual
      "An equation with binds of size greater then one should combine the first bind of the left\
      \ side with all binds of the right side."
      (
        Just [
          (
            testSol
          , (BL [testBindA], BL [testBindC]):(BL [testBindB], BL [testBindD]):testGamma
          )
        , (
            testSol
          , (BL [testBindA], BL [testBindD]):(BL [testBindB], BL [testBindC]):testGamma
          )  
        ]
      )
      (
        rule6
          testSol 
          ((BL [testBindA, testBindB], BL [testBindC, testBindD]):testGamma)
      )
  )

rule6EquationWithBindOfSizeTwoOnRightSide =
  TestCase (
    assertEqual
      "An equation with one bind of size one on the left side and one bind of size two on the right side should combine the first bind of the left\
      \ side with all binds of the right side."
      (
        Just [
          (
            testSol
          , (BL [testBindA], BL [testBindC]):(BL [], BL [testBindD]):testGamma
          )
        , (
            testSol
          , (BL [testBindA], BL [testBindD]):(BL [], BL [testBindC]):testGamma
          )  
        ]
      )
      (
        rule6
          testSol 
          ((BL [testBindA], BL [testBindC, testBindD]):testGamma)
      )
  )

rule6EquationWithBindOfSizeTwoOnLeftSide =
  TestCase (
    assertEqual
      "An equation with one bind of size one on the right side and one bind of size two on the left side should combine the first bind of the left\
      \ side with the bind of the right side."
      (
        Just [
          (
            testSol
          , (BL [testBindA], BL [testBindC]):(BL [testBindB], BL []):testGamma
          )
        ] 
      )
      (
        rule6
          testSol 
          ((BL [testBindA, testBindB], BL [testBindC]):testGamma)
      )
  )

rule6EquationWithBindOfSizeZeroOnLeftSide =
  TestCase (
    assertEqual
      "An equation with one bind of size zero on the left side and one bind of size two on the right side should resolve to nothing."
      Nothing
      (
        rule6
          testSol 
          ((BL [], BL [testBind, testBind]):testGamma)
      )
  )

rule6EquationWithBindOfSizeZeroOnRightSide =
  TestCase (
    assertEqual
      "An equation with one bind of size zero on the right side and one bind of size two on the left side should resolve to nothing."
      Nothing
      (
        rule6
          testSol 
          ((BL [testBind, testBind], BL []):testGamma)
      )
  )
  
rule6EquationWithTwoBindsOfSizeOne =
  TestCase (
    assertEqual
      "An equation with two binds of size one should resolve to nothing."
      Nothing
      (
        rule6
          testSol 
          ((BL [testBind], BL [testBind]):testGamma)
      )
  )

rule6EquationWithTwoBindsOfSizeZero =
  TestCase (
    assertEqual
      "An equation with two binds of size zero should resolve to nothing."
      Nothing
      (
        rule6
          testSol 
          ((BL [], BL []):testGamma)
      )
  )

rule6EquationWithTwoVariables =
  TestCase (
    assertEqual
      "An equation with two variables should resolve to nothing."
      Nothing
      (
        rule6
          testSol 
          ((V testVar, V testVar):testGamma)
      )
  )

-------------------------------
-- Rule 7 ---------------------
-------------------------------
rule7EquationWithBindOfSizeOneOnLeftSide =
  TestCase (
    assertEqual
      "An equation with a bind of size one on the left side and bind of size zero on the right side\
      \ should transform the current solution to nothing."
      (
        Just (
          Nothing
        , ((BL [testBind], BL []):testGamma)
        )
      )
      (
        rule7 
          testSol 
          ((BL [testBind], BL []):testGamma)
      )
  )

rule7EquationWithBindOfSizeTwoOnLeftSide =
  TestCase (
    assertEqual
      "An equation with a bind of size two on the left side and bind of size zero on the right side\
      \ should transform the current solution to nothing."
      (
        Just (
          Nothing
        , (((BL [testBind, testBind]), BL []):testGamma)
        )
      )
      (
        rule7 
          testSol 
          (((BL [testBind, testBind]), BL []):testGamma)
      )
  )

rule7EquationWithBindOfSizeZeroOnTheLeftSide =
  TestCase (
    assertEqual
      "An equation with a bind of size zero on the left side should resolve to nothing."
      Nothing
      (
        rule7 
          testSol 
          ((BL [], BL [testBind]):testGamma)
      )
  )

rule7EquationWithTwoBindsOfSizeZero =
  TestCase (
    assertEqual
      "An equation with two binds of size zero should resolve to nothing."
      Nothing
      (
        rule7 
          testSol 
          ((BL [], BL []):testGamma)
      )
  )

rule7EquationWithTwoBinds =
  TestCase (
    assertEqual
      "An equation with two binds should resolve to nothing."
      Nothing
      (
        rule7 
          testSol 
          ((BL [testBind], BL [testBind]):testGamma)
      )
  )

rule7EquationWithVarOnEachSide =
  TestCase (
    assertEqual
      "An equation with a variable on each side should resolve to nothing."
      Nothing
      (
        rule7 
          testSol 
          ((V testVar, V testVar):testGamma)
      )
  )

-------------------------------
-- Rule 8 ---------------------
-------------------------------
rule8EquationWithBindOfSizeOneOnRightSide =
  TestCase (
    assertEqual
      "An equation with a bind of size one on the right side and bind of size zero on the left side\
      \ should transform the current solution to nothing."
      (
        Just (
          Nothing
        , ((BL [], BL [testBind]):testGamma)
        )
      )
      (
        rule8
          testSol 
          ((BL [], BL [testBind]):testGamma)
      )
  )

rule8EquationWithBindOfSizeTwoOnLeftSide =
  TestCase (
    assertEqual
      "An equation with a bind of size two on the left side and bind of size zero on the right side\
      \ should transform the current solution to nothing."
      (
        Just (
          Nothing
        , ((BL [], (BL [testBind, testBind])):testGamma)
        )
      )
      (
        rule8 
          testSol 
          ((BL [], (BL [testBind, testBind])):testGamma)
      )
  )

rule8EquationWithBindOfSizeZeroOnTheRightSide =
  TestCase (
    assertEqual
      "An equation with a bind of size zero on the right side should resolve to nothing."
      Nothing
      (
        rule8 
          testSol 
          ((BL [testBind], BL []):testGamma)
      )
  )

rule8EquationWithTwoBindsOfSizeZero =
  TestCase (
    assertEqual
      "An equation with two binds of size zero should resolve to nothing."
      Nothing
      (
        rule8 
          testSol 
          ((BL [], BL []):testGamma)
      )
  )

rule8EquationWithTwoBinds =
  TestCase (
    assertEqual
      "An equation with two binds should resolve to nothing."
      Nothing
      (
        rule8 
          testSol 
          ((BL [testBind], BL [testBind]):testGamma)
      )
  )

rule8EquationWithVarOnEachSide =
  TestCase (
    assertEqual
      "An equation with a variable on each side should resolve to nothing."
      Nothing
      (
        rule8 
          testSol 
          ((V testVar, V testVar):testGamma)
      )
  )

-------------------------------
-- Rule 9 ---------------------
-------------------------------
rule9EquationWithTwoBindsOfSizeZero =
  TestCase (
    assertEqual
      "An equation with two binds of size zero should ."
      (Just (testSol, testGamma))
      (
        rule9
          testSol 
          ((BL [], BL []):testGamma)
      )
  )

rule9EquationWithBindOfSizeZeroOnTheLeftSide =
  TestCase (
    assertEqual
      "An equation with a bind of size zero on the left side should resolve to nothing."
      Nothing
      (
        rule9
          testSol 
          ((BL [], BL [testBind]):testGamma)
      )
  )

rule9EquationWithBindOfSizeZeroOnTheRightSide =
  TestCase (
    assertEqual
      "An equation with a bind of size zero on the right side should resolve to nothing."
      Nothing
      (
        rule9 
          testSol 
          ((BL [testBind], BL []):testGamma)
      )
  )

rule9EquationWithTwoBinds =
  TestCase (
    assertEqual
      "An equation with two binds should resolve to nothing."
      Nothing
      (
        rule9 
          testSol 
          ((BL [testBind], BL [testBind]):testGamma)
      )
  )

rule9EquationWithVarOnEachSide =
  TestCase (
    assertEqual
      "An equation with a variable on each side should resolve to nothing."
      Nothing
      (
        rule9 
          testSol 
          ((V testVar, V testVar):testGamma)
      )
  )

-------------------------------
-- Tests ----------------------
-------------------------------
testsRule1 = 
  TestList [
    TestLabel "rule1_test1" rule1EquationEachSideBindOfSizeOne
    , TestLabel "rule1_test2" rule1EquationWithBindOfSizeTwoOnRightSide
    , TestLabel "rule1_test3" rule1EquationWithBindOfSizeZeroOnTheRightSide
    , TestLabel "rule1_test4" rule1EquationWithBindOfSizeTwoOnLeftSide
    , TestLabel "rule1_test5" rule1EquationWithBindOfSizeZeroOnTheLeftSide
    , TestLabel "rule1_test6" rule1EquationWithVarOnEachSide
  ]

testsRule2 = 
  TestList [
    TestLabel "rule2_test1" rule2EquationWithTwoIdenticalConcretes
    , TestLabel "rule2_test2" rule2EquationWithTwoIdenticalMetas
    , TestLabel "rule2_test3" rule2EquationWithTwoDifferentConcretes
    , TestLabel "rule2_test4" rule2EquationWithTwoDifferentMetas
    , TestLabel "rule2_test5" rule2EquationWithMetaOnTheLeftAndConcreteOnTheRight
    , TestLabel "rule2_test6" rule2EquationWithConcreteOnTheLeftAndMetaOnTheRight
    , TestLabel "rule2_test7" rule2EquationWithTwoBinds
  ]

testsRule3 = 
  TestList [
    TestLabel "rule3_test1" rule3EquationWithTwoDifferentConcretes
    , TestLabel "rule3_test2" rule3EquationWithTwoIdenticalConcretes
    , TestLabel "rule3_test3" rule3EquationWithTwoIdenticalMetas
    , TestLabel "rule3_test4" rule3EquationWithTwoDifferentMetas
    , TestLabel "rule3_test5" rule3EquationWithTwoBinds
    , TestLabel "rule3_test6" rule3EquationWithConcreteOnTheLeftAndMetaOnTheRight
    , TestLabel "rule3_test7" rule3EquationWithMetaOnTheLeftAndConcreteOnTheRight
    
  ]

testsRule4 = 
  TestList [
    TestLabel "rule4_test1" rule4EquationWithMetaOnTheLeftAndConcreteOnTheRight
    , TestLabel "rule4_test2" rule4EquationWithConcreteOnTheLeftAndMetaOnTheRight
    , TestLabel "rule4_test3" rule4EquationWithTwoMetas
    , TestLabel "rule4_test4" rule4EquationWithTwoConcretes
    , TestLabel "rule4_test5" rule4EquationWithTwoBinds
    
  ]

testsRule5 = 
  TestList [
    TestLabel "rule5_test1" rule5EquationWithTwoDiffentMetas
    , TestLabel "rule5_test2" rule5EquationWithTwoIdenticalMetas
    , TestLabel "rule5_test3" rule5EquationWithTwoIdenticalConcretes
    , TestLabel "rule5_test4" rule5EquationWithTwoDifferentConcretes
    , TestLabel "rule5_test5" rule5EquationWithTwoBinds
    , TestLabel "rule5_test6" rule5EquationWithConcreteOnTheLeftAndMetaOnTheRight
    , TestLabel "rule5_test7" rule5EquationWithMetaOnTheLeftAndConcreteOnTheRight
  ]

testsRule6 = 
  TestList [
    TestLabel "rule6_test1" rule6EquationWithBindsGreaterThenOneOnBothSides
    , TestLabel "rule6_test2" rule6EquationWithBindOfSizeTwoOnRightSide
    , TestLabel "rule6_test3" rule6EquationWithBindOfSizeTwoOnLeftSide
    , TestLabel "rule6_test4" rule6EquationWithBindOfSizeZeroOnLeftSide
    , TestLabel "rule6_test5" rule6EquationWithBindOfSizeZeroOnRightSide
    , TestLabel "rule6_test6" rule6EquationWithTwoBindsOfSizeOne
    , TestLabel "rule6_test7" rule6EquationWithTwoBindsOfSizeZero
    , TestLabel "rule6_test8" rule6EquationWithTwoVariables
  ]

testsRule7 = 
  TestList [
    TestLabel "rule7_test1" rule7EquationWithBindOfSizeOneOnLeftSide
    , TestLabel "rule7_test2" rule7EquationWithBindOfSizeTwoOnLeftSide
    , TestLabel "rule7_test3" rule7EquationWithBindOfSizeZeroOnTheLeftSide
    , TestLabel "rule7_test4" rule7EquationWithTwoBindsOfSizeZero
    , TestLabel "rule7_test5" rule7EquationWithTwoBinds
    , TestLabel "rule7_test6" rule7EquationWithVarOnEachSide
  ]

testsRule8 = 
  TestList [
    TestLabel "rule7_test1" rule8EquationWithBindOfSizeOneOnRightSide
    , TestLabel "rule7_test2" rule8EquationWithBindOfSizeTwoOnLeftSide
    , TestLabel "rule7_test3" rule8EquationWithBindOfSizeZeroOnTheRightSide
    , TestLabel "rule7_test4" rule8EquationWithTwoBindsOfSizeZero
    , TestLabel "rule7_test5" rule8EquationWithTwoBinds
    , TestLabel "rule7_test6" rule8EquationWithVarOnEachSide
  ]

testsRule9 = 
  TestList [
    TestLabel "rule9_test1" rule9EquationWithTwoBindsOfSizeZero
    , TestLabel "rule9_test2" rule9EquationWithBindOfSizeZeroOnTheLeftSide
    , TestLabel "rule9_test3" rule9EquationWithBindOfSizeZeroOnTheRightSide
    , TestLabel "rule9_test4" rule9EquationWithTwoBinds
    , TestLabel "rule9_test5" rule9EquationWithVarOnEachSide
  ]

main = do
  putStrLn "Rule1"
  runTestTT testsRule1
  putStrLn ""
  putStrLn "Rule2"
  runTestTT testsRule2
  putStrLn ""
  putStrLn "Rule3"
  runTestTT testsRule3
  putStrLn ""
  putStrLn "Rule4"
  runTestTT testsRule4
  putStrLn ""
  putStrLn "Rule5"
  runTestTT testsRule5
  putStrLn ""
  putStrLn "Rule6"
  runTestTT testsRule6
  putStrLn ""
  putStrLn "Rule7"
  runTestTT testsRule7
  putStrLn ""
  putStrLn "Rule8"
  runTestTT testsRule8
  putStrLn ""
  putStrLn "Rule9"
  runTestTT testsRule9
  putStrLn ""