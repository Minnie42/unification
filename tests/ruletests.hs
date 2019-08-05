import Test.HUnit
import Types
import Rules

testVar :: Var
testVar = Meta "Var"

testVarX :: Var
testVarX = Meta "VarX"

testVarY :: Var
testVarY = Meta "VarY"

testBind :: Bind
testBind = B testVar testVar

testGamma :: Problem
testGamma = []

testSol :: Maybe Sol
testSol = Just []

-------------------------------
-- Rule 1 ---------------------
-------------------------------
rule1EquationEachSideBindOfSizeOne =
  TestCase (
    assertEqual
      "An equation with a bind of size one on each side should combine the first variable\
      \of the left bind with the first variable of the right bind and the second variable of\
      \the right bind with the second variable of the left bind."
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
twoSameConcretesRule2 = 
  TestCase (
    assertEqual
      "two identical concretes should be removed"
      (Just (sol, []))
      (rule2 sol problem)
  )
  where
    sol = Just []
    problem = [(V (Concrete "x"), V (Concrete "x"))]

twoVariablesMetaAndConcreteRule2 = 
  TestCase (
    assertEqual
      "anything then a concrete should resolve to Nothing"
      Nothing
      (rule2 sol problem)
  )
  where
    sol = Just []
    problem = [(V (Meta "X"), V (Concrete "x"))]

twoinputsBindAndSomethingRule2 = 
  TestCase (
    assertEqual
      "anything then a concrete should resolve to Nothing"
      Nothing
      (rule2 sol problem)
  )
  where
    sol = Just []
    problem = [(BL [B (Meta "X") (Concrete "x")], V (Meta "X"))]

-------------------------------
-- Rule 3 ---------------------
-------------------------------
twoDifferentConcretesRule3 = 
  TestCase (
    assertEqual
      "two different concretes should resolve to an error"
      (Just (Nothing, problem))
      (rule3 sol problem)
  )
  where
    sol = Just []
    problem = [(V (Concrete "x"), V (Concrete "y"))]
  
twoVariablesMetaAndConcreteRule3 = 
  TestCase (
    assertEqual
      "anything then a concrete should resolve in Nothing"
      Nothing
      (rule3 sol problem)
  )
  where
    sol = Just []
    problem = [(V (Meta "X"), V (Concrete "x"))]

twoinputsBindAndSomethingRule3 = 
  TestCase (
    assertEqual
      "anything then a concrete should resolve in Nothing"
      Nothing
      (rule3 sol problem)
  )
  where
    sol = Just []
    problem = [(BL [B (Meta "X") (Concrete "x")], V (Meta "X"))]

-------------------------------
-- Rule 4 ---------------------
-------------------------------	
twoVariablesMetaAndConcreteRule4 =
  TestCase (
    assertEqual
      "the meta variable should be substituted with the concrete variable"
      (Just (Just [(Meta "X",Concrete "x")], [(V (Concrete "x"), V (Concrete "x"))]))
      (rule4 sol problem)
  )
  where
    sol = Just []
    problem = [(V (Meta "X"), V (Concrete "x"))]

twoBindsWithTwoVariablesMetaAndConcreteRule4 =
  TestCase (
    assertEqual
      "the meta variable should be substituted with the concrete variable and again with the next concrete variable"
      (Just (Just [(Meta "X", Concrete "x")], [(V (Concrete "x"), V (Concrete "x")), (V (Concrete "x"), V (Concrete "y"))]))
      (rule4 sol problem)
  )
  where
    sol = Just []
    problem = [(V (Meta "X"), V (Concrete "x")), (V (Meta "X"), V (Concrete "y"))]

twoVariablesConcreteAndMetaRule4 =
  TestCase (
    assertEqual
      "?"--TODO
      (Just (Just [(Meta "X",Concrete "x")], [(V (Concrete "x"), V (Concrete "x"))]))
      (rule4 sol problem)
  )
  where
    sol = Just []
    problem = [(V (Concrete "x"), V (Meta "X"))]

twoMetaVariablesRule4 =
  TestCase (
    assertEqual
      "anything then a meta and a concrete variable should resolve to Nothing"
      Nothing
      (rule4 sol problem)
  )
  where
    sol = Just []
    problem = [(V (Meta "X"), V (Meta "Y"))]

-------------------------------
-- Rule 5 ---------------------
-------------------------------
twoMetaVariablesRule5 =
  TestCase (
    assertEqual
      "the first meta variable should be substituted with the second meta variable"
      (Just (Just [(Meta "X",Meta "Y")], [(V (Meta "Y"), V (Meta "Y"))]))
      (rule5 sol problem)
  )
  where
    sol = Just []
    problem = [(V (Meta "X"), V (Meta "Y"))]

twoBindsWithTwoMetaVariablesRule5 =
  TestCase (
    assertEqual
      "the first meta variable should be substituted with the other meta variable and again with the next meta variable"
      (Just (Just [(Meta "X", Meta "Y")], [(V (Meta "Y"), V (Meta "Y")), (V (Meta "Y"), V (Meta "Z"))]))
      (rule5 sol problem)
  )
  where
    sol = Just []
    problem = [(V (Meta "X"), V (Meta "Y")), (V (Meta "X"), V (Meta "Z"))]

twoVariablesConcreteAndMetaRule5 =
  TestCase (
    assertEqual
      "anything then two meta variables should resolve to Nothing"
      Nothing
      (rule5 sol problem)
  )
  where
    sol = Just []
    problem = [(V (Concrete "x"), V (Meta "X"))]

twoVariablesMetaAndConcreteRule5 =
  TestCase (
    assertEqual
      "anything then two meta variables should resolve to Nothing"
      Nothing
      (rule5 sol problem)
  )
  where
    sol = Just []
    problem = [(V (Meta "X"), V (Concrete "x"))]

-------------------------------
-- Rule 6 ---------------------
-------------------------------
equationWithTwoListsOfOneBindEachRule6 =
  TestCase (
    assertEqual
      "equation with two lists of one bind each should resolve to the same equation and a equation of two empty binds"
      Nothing      
      (rule6 sol problem)
  )
  where
    sol = Just []
    problem = [
      (
        BL [B (Meta "X") (Concrete "x")],
        BL [B (Meta "Y") (Concrete "y")]
      )]

equationWithTwoListsOfTwoBindsEachRule6 = 
  TestCase (
    assertEqual
      "equation with two lists of two binds each should resolve ? and a equation of two empty binds" --TODO
      (Just [(sol, [(BL [B (Meta "X") (Concrete "x")], BL [B (Concrete "y") (Concrete "x")]), (BL [B (Meta "Y") (Concrete "y")], BL [B (Meta "Y") (Concrete "z")])]),
      (sol, [(BL [B (Meta "X") (Concrete "x")], BL [B (Meta "Y") (Concrete "z")]), (BL [B (Meta "Y") (Concrete "y")], BL [B (Concrete "y") (Concrete "x")])])])
      (rule6 sol problem)
  )
  where
    sol = Just []
    problem = [
      (
        (BL [(B (Meta "X") (Concrete "x")), (B (Meta "Y") (Concrete "y"))]),
        (BL [(B (Concrete "y") (Concrete "x")), (B (Meta "Y") (Concrete "z"))])
      )]

equationWithTwoListOfBindsOneEmptyRule6 =
  TestCase (
    assertEqual
      "?" --TODO: comment
      Nothing
      (rule6 sol problem)
  )
  where
    sol = Just []
    problem = [((BL [(B (Meta "X") (Concrete "x")), (B (Meta "Y") (Concrete "y"))]), BL [])]

equationOfTwoListOfBindsSecondLongerRule6 =
  TestCase (
    assertEqual
    "?" --TODO: comment
    (Just [(sol, [(BL [B (Meta "X") (Concrete "x")], BL [B (Concrete "y") (Concrete "x")]), (BL [B (Meta "Y") (Concrete "y")], BL [])])])
    (rule6 sol problem)
  )
  where
    sol = Just []
    problem = [
      (
        (BL [(B (Meta "X") (Concrete "x")), (B (Meta "Y") (Concrete "y"))]),
        (BL [(B (Concrete "y") (Concrete "x"))])
      )]

equationOfTwoListOfBindsFirstLongerRule6 =
  TestCase (
    assertEqual
    "?" --TODO: comment
    (Just [(sol, [((BL [B (Concrete "y") (Concrete "x")]), (BL [B (Meta "X") (Concrete "x")])), ((BL []), (BL [B (Meta "Y") (Concrete "y")]))]),
    (sol, [((BL [B (Concrete "y") (Concrete "x")]), (BL [B (Meta "Y") (Concrete "y")])), ((BL []), (BL [B (Meta "X") (Concrete "x")]))])])
    (rule6 sol problem)
  )
  where
    sol = Just []
    problem = [
      (
        (BL [(B (Concrete "y") (Concrete "x"))]),
        (BL [(B (Meta "X") (Concrete "x")), (B (Meta "Y") (Concrete "y"))])
      )]

equationWithOneListOfBindsAndOneVariableRule6 =
  TestCase (
    assertEqual
      "?" --TODO: comment
      Nothing
      (rule6 sol problem)
  )
  where
    sol = Just []
    problem = [((BL [(B (Meta "X") (Concrete "x")), (B (Meta "Y") (Concrete "y"))]), V (Concrete "x"))]

-------------------------------
-- Rule 7 ---------------------
-------------------------------
equationWithOneEmptyBindRule7 = 
  TestCase (
    assertEqual
      "equation with one list of binds and one empty bind should resolve to an error"
      (Just (Nothing, problem))
      (rule7 sol problem)
  )
  where
    sol = Just []
    problem = [(BL ((B (Meta "X") (Concrete "x")):(B (Meta "Y") (Concrete "y")):(B (Meta "Z") (Concrete "z")):[]), BL [])]

equationWithOneEmptyBindOtherSideRule7 = 
  TestCase (
    assertEqual
      "equation with one empty bind on the right and one list of binds should resolve to Nothing"
      Nothing
      (rule7 sol problem)
  )
  where
    sol = Just []
    problem = [(BL [], BL ((B (Meta "X") (Concrete "x")):(B (Meta "Y") (Concrete "y")):(B (Meta "Z") (Concrete "z")):[]))]

equationWithtwoEmptyBindsRule7 =
  TestCase (
    assertEqual
      "equation with two empty binds should resolved to Nothing" 
      Nothing
      (rule7 sol problem)
  )
  where
    sol = Just []
    problem = [(BL [], BL [])]

equationWithTwoBindsRule7 =
  TestCase (
    assertEqual
      "equation with two binds should resolved to Nothing" 
      Nothing
      (rule7 sol problem)
  )
  where
    sol = Just []
    problem = [(BL [B (Meta "Y") (Concrete "y")], BL [B (Meta "X") (Concrete "x")])]

-------------------------------
-- Rule 8 ---------------------
-------------------------------
equationWithOneEmptyBindRule8 = 
  TestCase (
    assertEqual
      "equation with one empty bind and one list of binds should resolve to an error"
      (Just (Nothing, problem))
      (rule8 sol problem)
  )
  where
    sol = Just []
    problem = [(BL [], BL ((B (Meta "X") (Concrete "x")):(B (Meta "Y") (Concrete "y")):(B (Meta "Z") (Concrete "z")):[]))]

equationWithOneEmptyBindOtherSideRule8 = 
  TestCase (
    assertEqual
      "equation with one list of binds and one empty bind on the left should resolve to Nothing"
      Nothing
      (rule8 sol problem)
  )
  where
    sol = Just []
    problem = [(BL ((B (Meta "X") (Concrete "x")):(B (Meta "Y") (Concrete "y")):(B (Meta "Z") (Concrete "z")):[]), BL [])]

equationWithtwoEmptyBindsRule8 =
  TestCase (
    assertEqual
      "equation with two empty binds should resolved to Nothing" 
      Nothing
      (rule8 sol problem)
  )
  where
    sol = Just []
    problem = [(BL [], BL [])]

equationWithTwoBindsRule8 =
  TestCase (
    assertEqual
      "equation with two binds should resolved to Nothing" 
      Nothing
      (rule8 sol problem)
  )
  where
    sol = Just []
    problem = [(BL [B (Meta "Y") (Concrete "y")], BL [B (Meta "X") (Concrete "x")])]

-------------------------------
-- Rule 9 ---------------------
-------------------------------
equationWithTwoEmptyBindsRule9 =
  TestCase (
    assertEqual
      "equation with two empty binds should resolved to two empty lists"
      (Just (sol, []))
      (rule9 sol problem)
  )
  where
    sol = Just []
    problem = [(BL [], BL [])]

equationWithOneBindAndAVariableRule9 = 
  TestCase (
    assertEqual
      "equation with one bind and one variable should resolve in Nothing"
      Nothing
      (rule1 sol problem)
  )
  where
    sol = Just []
    problem = [(BL [B (Meta "X") (Concrete "x")], V (Meta "X"))]

equationWithTwoBindsRule9 =
  TestCase (
    assertEqual
      "equation with two non empty binds should resolve in Nothing"
      Nothing
      (rule1 sol problem)
  )
  where
    sol = Just []
    problem = [(V (Meta "X"), V (Concrete "x"))]

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
    TestLabel "rule2_test1" twoSameConcretesRule2
    , TestLabel "rule2_test2" twoVariablesMetaAndConcreteRule2
    , TestLabel "rule2_test3" twoinputsBindAndSomethingRule2
  ]

testsRule3 = 
  TestList [
    TestLabel "rule3_test1" twoDifferentConcretesRule3
    , TestLabel "rule3_test2" twoVariablesMetaAndConcreteRule3
    , TestLabel "rule3_test3" twoinputsBindAndSomethingRule3
  ]

testsRule4 = 
  TestList [
    TestLabel "rule4_test1" twoVariablesMetaAndConcreteRule4
    , TestLabel "rule4_test2" twoBindsWithTwoVariablesMetaAndConcreteRule4
    , TestLabel "rule4_test3" twoVariablesConcreteAndMetaRule4
    , TestLabel "rule4_test4" twoMetaVariablesRule4
  ]

testsRule5 = 
  TestList [
    TestLabel "rule5_test1" twoMetaVariablesRule5
    , TestLabel "rule5_test2" twoBindsWithTwoMetaVariablesRule5
    , TestLabel "rule5_test2" twoVariablesConcreteAndMetaRule5
    , TestLabel "rule5_test4" twoVariablesMetaAndConcreteRule5
  ]

testsRule6 = 
  TestList [
    TestLabel "rule6_test1" equationWithTwoListsOfOneBindEachRule6
    , TestLabel "rule6_test2" equationWithTwoListsOfTwoBindsEachRule6
    , TestLabel "rule6_test3" equationWithTwoListOfBindsOneEmptyRule6
    , TestLabel "rule6_test4" equationOfTwoListOfBindsSecondLongerRule6
    , TestLabel "rule6_test5" equationOfTwoListOfBindsFirstLongerRule6
    , TestLabel "rule6_test6" equationWithOneListOfBindsAndOneVariableRule6
  ]

testsRule7 = 
  TestList [
    TestLabel "rule7_test1" equationWithOneEmptyBindRule7
    , TestLabel "rule7_test2" equationWithtwoEmptyBindsRule7
    , TestLabel "rule7_test3" equationWithTwoBindsRule7
    , TestLabel "rule7_test4" equationWithOneEmptyBindOtherSideRule7
  ]

testsRule8 = 
  TestList [
    TestLabel "rule8_test1" equationWithOneEmptyBindRule8
    , TestLabel "rule8_test2" equationWithtwoEmptyBindsRule8
    , TestLabel "rule8_test3" equationWithTwoBindsRule8
    , TestLabel "rule8_test4" equationWithOneEmptyBindOtherSideRule8
  ]

testsRule9 = 
  TestList [
    TestLabel "rule9_test1" equationWithTwoEmptyBindsRule9
    , TestLabel "rule9_test2" equationWithOneBindAndAVariableRule9
    , TestLabel "rule9_test3" equationWithTwoBindsRule9
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