import Test.HUnit
import Types
import Rules

-------------------------------
-- Rule 1 ---------------------
-------------------------------
equationWithTwoBindsRule1 =
  TestCase (
    assertEqual
      "equation with two binds should be resolved to two equation with two variables each"
      (Just (sol, [(V (Meta "X"), V (Meta "X")), (V (Concrete "x"), V (Concrete "x"))]))
      (rule1 sol problem)
  )
  where
    sol = Just []
    problem = [(BL [B (Meta "X") (Concrete "x")], BL [B (Meta "X") (Concrete "x")])]

equationWithOneBindAndAVariableRule1 = 
  TestCase (
    assertEqual
      "equation with one bind and one variable should be resolved in Nothing"
      Nothing
      (rule1 sol problem)
  )
  where
    sol = Just []
    problem = [(BL [B (Meta "X") (Concrete "x")], V (Meta "X"))]

twoEmptyBindsRule1 =
  TestCase (
    assertEqual
      "equation with two empty binds should resolved to Nothing" 
      Nothing
      (rule1 sol problem)
  )
  where
    sol = Just []
    problem = [(BL [], BL [])]

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
      (Just (Just [(Meta "X",Concrete "x")], []))
      (rule4 sol problem)
  )
  where
    sol = Just []
    problem = [(V (Meta "X"), V (Concrete "x"))]

twoBindsWithTwoVariablesMetaAndConcreteRule4 =
  TestCase (
    assertEqual
      "the meta variable should be substituted with the concrete variable and again with the next concrete variable"
      (Just (Just [(Meta "X", Concrete "x")], [(V (Concrete "x"), V (Concrete "y"))]))
      (rule4 sol problem)
  )
  where
    sol = Just []
    problem = [(V (Meta "X"), V (Concrete "x")), (V (Meta "X"), V (Concrete "y"))]

twoVariablesConcreteAndMetaRule4 =
  TestCase (
    assertEqual
      "anything then a meta and a concrete variable should resolve to Nothing"
      Nothing
      (rule4 sol problem)
  )
  where
    sol = Just []
    problem = [(V (Concrete "X"), V (Meta "x"))]

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
      (Just (Just [(Meta "X",Meta "Y")], []))
      (rule5 sol problem)
  )
  where
    sol = Just []
    problem = [(V (Meta "X"), V (Meta "Y"))]

twoBindsWithTwoMetaVariablesRule5 =
  TestCase (
    assertEqual
      "the first meta variable should be substituted with the other meta variable and again with the next meta variable"
      (Just (Just [(Meta "X",Meta "Y")],[(V (Meta "Y"),V (Meta "Z"))]))
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
      (rule4 sol problem)
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
    TestLabel "rule1_test1" equationWithTwoBindsRule1
    , TestLabel "rule1_test2" equationWithOneBindAndAVariableRule1
    , TestLabel "rule1_test3" twoEmptyBindsRule1
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
  putStrLn "Rule2"
  runTestTT testsRule2
  putStrLn "Rule3"
  runTestTT testsRule3
  putStrLn "Rule4"
  runTestTT testsRule4
  putStrLn "Rule5"
  runTestTT testsRule5
  putStrLn "Rule6"
  runTestTT testsRule6
  putStrLn "Rule7"
  runTestTT testsRule7
  putStrLn "Rule8"
  runTestTT testsRule8
  putStrLn "Rule9"
  runTestTT testsRule9
  putStrLn "Done"