import Test.HUnit
import Types
import Util
import Tests.DummyVariables


-----------------------------------
-- applySubstitutionToMeta --------
-----------------------------------
substituteMetaToConcrete =
  TestCase (
    assertEqual
      "Substitute a meta variable with a substitution that starts with this meta variable and ends\
      \ with a concrete variable should return this concrete variable."
      testConcreteX
      (applySubstitutionToMeta (testMetaX, testConcreteX) testMetaX)
  )

substituteMetaToMeta =
  TestCase (
    assertEqual
      "Substitute a meta variable with a substitution that starts with this meta variable and ends\
      \ with another meta variable should return the second meta variable."
      testMetaY
      (applySubstitutionToMeta (testMetaX, testMetaY) testMetaX)
  )

twoDifferentMetasNoSubstitution =
  TestCase (
    assertEqual
      "If the left side of the substitution doesn't match the variable no substitution should happen."
      testMetaY
      (applySubstitutionToMeta (testMetaX, testVar) testMetaY)
  )

concreteNoSubstitution =
  TestCase (
    assertEqual
      "If we get a concrete as variable to be substituted no substitution should happen."
      testConcreteX
      (applySubstitutionToMeta (testMeta, testVar) testConcreteX)
  )

-----------------------------------
-- applySubstitutionToBind --------
-----------------------------------
substitutionInBind =
  TestCase (
    assertEqual
      "Given a Substitution that applies in the given bind the substitution should apply."
      (B testConcreteY testMetaY)
      (applySubstitutionToBind (testMetaX, testConcreteY) (B testMetaX testMetaY))
  )
  
noSubstitutionInBind =
  TestCase (
    assertEqual
      "Given a Substitution that doesn't apply in the given bind no substitution should apply."
      (B testMetaY testMetaY)
      (applySubstitutionToBind (testMetaX, testConcreteY) (B testMetaY testMetaY))
  )

substitutionInChainVariable =
  TestCase (
    assertEqual
      "Given a Substitution that applies in the given chain variable the substitution should apply."
      (CV "test" testConcreteY testMetaY)
      (applySubstitutionToBind (testMetaX, testConcreteY) (CV "test" testMetaX testMetaY))
  )

-----------------------------------
-- applySubstitutionToSide --------
-----------------------------------
substitutionInVariable =
  TestCase (
    assertEqual
      "Given a Substitution that applies in the given side that is a variabe the substitution should apply."
      (V testConcreteY)
      (applySubstitutionToSide (testMetaX, testConcreteY) (V testMetaX))
  )

noSubstitutionInVariable =
  TestCase (
    assertEqual
      "Given a Substitution that doesn't apply in the given side that is a variabe no substitution should apply."
      (V testMetaY)
      (applySubstitutionToSide (testMetaX, testConcreteY) (V testMetaY))
  )

substitutionInBinds =
  TestCase (
    assertEqual
      "Given a Substitution that applies in the given side that is a bind of size two the substitution should apply."
      (BL [(B testConcreteY testMetaY), (B testMetaY testConcreteY)])
      (applySubstitutionToSide (testMetaX, testConcreteY) (BL [(B testMetaX testMetaY), (B testMetaY testMetaX)]))
  )

noSubstitutionInBinds =
  TestCase (
    assertEqual
      "Given a Substitution that doesn't apply in the given side that is a bind no substitution should apply."
      (BL [(B testMetaY testMetaY), (B testMetaY testMetaY)])
      (applySubstitutionToSide (testMetaX, testConcreteY) (BL [(B testMetaY testMetaY), (B testMetaY testMetaY)]))
  )
  
noSubstitutionWithEmptySet =
  TestCase (
    assertEqual
      "Given an empty set no substitution should apply."
      (BL [])
      (applySubstitutionToSide (testMetaX, testConcreteY) (BL []))
  )
  
-----------------------------------
-- applySubstitutionToEquation ----
-----------------------------------
substitutionInEquation =
  TestCase (
    assertEqual
      "Given a Substitution that applies in the given equation the substitution should apply."
      (BL [B testConcreteY testMetaY], BL [B testMetaY testConcreteY])
      (applySubstitutionToEquation (testMetaX, testConcreteY) (BL [B testMetaX testMetaY], BL [B testMetaY testMetaX]))
  )
  
noSubstitutionInEquation =
  TestCase (
    assertEqual
      "Given a Substitution that doesn't apply in the given equation no substitution should apply."
      (BL [B testMetaY testMetaY], BL [B testMetaY testMetaY])
      (applySubstitutionToEquation (testMetaX, testConcreteY) (BL [B testMetaY testMetaY], BL [B testMetaY testMetaY]))
  )

-----------------------------------	
-- applySubstitutionToGamma -------
-----------------------------------
substitutionInGamma =
  TestCase (
    assertEqual
      "Given a Substitution that applies in the given gamma the substitution should apply."
      [(BL [B testConcreteY testMetaY], BL [B testMetaY testConcreteY]), (V testConcreteY, V testVarX)]
      (
        applySubstitutionToGamma 
          (testMetaX, testConcreteY) 
          [(BL [B testMetaX testMetaY], BL [B testMetaY testMetaX]), (V testMetaX, V testVarX)]
      )
  )
  
noSubstitutionInGamma =
  TestCase (
    assertEqual
      "Given a Substitution that doesn't apply in the given gamma no substitution should apply."
      [(BL [B testMetaY testMetaY], BL [B testMetaY testMetaY]), (V testMetaY, V testVarX)]
      (
        applySubstitutionToGamma 
          (testMetaX, testConcreteY) 
          [(BL [B testMetaY testMetaY], BL [B testMetaY testMetaY]), (V testMetaY, V testVarX)]
      )
  )

-----------------------------------	
-- applySolutionToGamma -------
-----------------------------------
solutionEmptyList =
  TestCase (
    assertEqual
      "Given an empty solution gamma shouldn't change."
      [(V testMetaX, V testMetaX)]
      (
        applySolutionToGamma 
          [] 
          [(V testMetaX, V testMetaX)]
      )
  )
  
solutionWithTwoIndependentSubstitutions =
  TestCase (
    assertEqual
      "Given a Solution with two independent substitutions should apply the substitutions seperately."
      [(BL [B testConcreteX testConcreteY], BL [B testConcreteY testConcreteY]), (V testConcreteY, V testConcreteX)]
      (
        applySolutionToGamma 
          [(testMetaX, testConcreteX), (testMetaY, testConcreteY)] 
          [(BL [B testMetaX testMetaY], BL [B testMetaY testMetaY]), (V testMetaY, V testMetaX)]
      )
  )

solutionWithTwoDependentSubstitutions =
  TestCase (
    assertEqual
      "Given a Solution with two dependent substitutions should apply the first substitutions correctly and the second\
      \ correctly considering the previous changes."
      [(BL [B testConcreteY testConcreteY], BL [B testConcreteY testConcreteY]), (V testConcreteY, V testConcreteY)]
      (
        applySolutionToGamma 
          [(testMetaY, testConcreteY), (testMetaX, testMetaY)] 
          [(BL [B testMetaX testMetaY], BL [B testMetaY testMetaY]), (V testMetaY, V testMetaX)]
      )
  )

-----------------------------------
-- unJust -------------------------
-----------------------------------
unJustApplies =
  TestCase (
    assertEqual
      "Given a value wrapped in a just it removes the just."
      (value)
      (unJust (Just value))
  )
  where
    value = 10

-----------------------------------
-- isProblemSolved ----------------
-----------------------------------
solvedProblem =
  TestCase (
    assertEqual
      "A solved problem should return true."
      True
      (isProblemSolved [(V testMetaX, V testMetaX)])
  )

unsolvedProblem =
  TestCase (
    assertEqual
      "A unsolved problem should return false."
      False
      (isProblemSolved [(V testMetaY, V testMetaX)])
  )
  
emptyProblem =
  TestCase (
    assertEqual
      "An empty problem should return true."
      True
      (isProblemSolved [])
  )

-----------------------------------
-- expandChainVariable ------------
-----------------------------------
chainVariableThatExpands =
  TestCase (
    assertEqual
      "Chain variable should expand correctly."
      [B testMetaX (Meta "CVA1"), B (Meta "CVA1") (Meta "CVA2"), B (Meta "CVA2") testMetaY]
      (expandChainVariable (CV "A" testMetaX testMetaY) 3)
  )

chainVariableThatNotExpands =
  TestCase (
    assertEqual
      "Given a chain size of one the chain variable should be converted to a bind."
      [B testMetaX testMetaY]
      (expandChainVariable (CV "A" testMetaX testMetaY) 1)
  )

-----------------------------------
-- expandChainVariablesInBinds ----
-----------------------------------
noExpansion =
  TestCase (
    assertEqual
      "."
      [[testBindA, testBindB]]
      (expandChainVariablesInBinds [testBindA, testBindB])
  )

oneExpansion =
  TestCase (
    assertEqual
      "."
      [
        [B testMetaX testMetaY, testBindB]
      , [B testMetaX (Meta "CVtest1"), B (Meta "CVtest1") testMetaY, testBindB]
      , [B testMetaX (Meta "CVtest1"), B (Meta "CVtest1") (Meta "CVtest2"), B (Meta "CVtest2") testMetaY, testBindB]
      ]
      (take 3 (expandChainVariablesInBinds [CV "test" testMetaX testMetaY, testBindB]))
  )


-------------------------------
-- Tests ----------------------
-------------------------------
testsApplySubstitutionToMeta = 
  TestList [
    TestLabel "applySubstitutionToMeta_test1" substituteMetaToConcrete
    , TestLabel "applySubstitutionToMeta_test2" substituteMetaToMeta
    , TestLabel "applySubstitutionToMeta_test3" twoDifferentMetasNoSubstitution
    , TestLabel "applySubstitutionToMeta_test4" concreteNoSubstitution
  ]

testsApplySubstitutionToBind =
  TestList [
    TestLabel "applySubstitutionToBind_test1" substitutionInBind
    , TestLabel "applySubstitutionToBind_test2" noSubstitutionInBind
    , TestLabel "applySubstitutionToBind_test3" substitutionInChainVariable
  ]

testsApplySubstitutionToSide =
  TestList [
    TestLabel "applySubstitutionToSide_test1" substitutionInVariable
    , TestLabel "applySubstitutionToSide_test2" noSubstitutionInVariable
    , TestLabel "applySubstitutionToSide_test3" substitutionInBinds
    , TestLabel "applySubstitutionToSide_test4" noSubstitutionInBinds
    , TestLabel "applySubstitutionToSide_test5" noSubstitutionWithEmptySet
    
  ]

testsApplySubstitutionToEquation =
  TestList [
    TestLabel "applySubstitutionToEquation_test1" substitutionInEquation
    , TestLabel "applySubstitutionToEquation_test2" noSubstitutionInEquation
  ]

testsApplySubstitutionToGamma =
  TestList [
    TestLabel "applySubstitutionToGamma_test1" substitutionInGamma
    , TestLabel "applySubstitutionToGamma_test2" noSubstitutionInGamma
  ]

testsApplySolutionToGamma =
  TestList [
    TestLabel "applySolutionToGamma_test1" solutionEmptyList
    , TestLabel "applySolutionToGamma_test2" solutionWithTwoIndependentSubstitutions
    , TestLabel "applySolutionToGamma_test3" solutionWithTwoDependentSubstitutions
    
  ]

testsUnJust =
  TestList [
    TestLabel "unJust_test1" unJustApplies
  ]

testsIsProblemSolved =
  TestList [
    TestLabel "isProblemSolved_test1" solvedProblem
    , TestLabel "isProblemSolved_test2" unsolvedProblem
    , TestLabel "isProblemSolved_test3" emptyProblem
  ]

testsExpandChainVariable =
  TestList [
    TestLabel "expandChainVariable_test1" chainVariableThatExpands
    , TestLabel "expandChainVariable_test2" chainVariableThatNotExpands
  ]

testsExpandChainVariablesInBinds =
  TestList [
    TestLabel "expandChainVariablesInBinds_test1" noExpansion
    , TestLabel "expandChainVariablesInBinds_test2" oneExpansion
  ]

main = do
  putStrLn "applySubstitutionToMeta"
  runTestTT testsApplySubstitutionToMeta
  putStrLn ""
  putStrLn "applySubstitutionToBind"
  runTestTT testsApplySubstitutionToBind
  putStrLn ""
  putStrLn "applySubstitutionToSide"
  runTestTT testsApplySubstitutionToSide
  putStrLn ""
  putStrLn "applySubstitutionToEquation"
  runTestTT testsApplySubstitutionToEquation
  putStrLn ""
  putStrLn "applySubstitutionToGamma"
  runTestTT testsApplySubstitutionToGamma
  putStrLn ""
  putStrLn "applySolutionToGamma"
  runTestTT testsApplySolutionToGamma
  putStrLn ""
  putStrLn "unJust"
  runTestTT testsUnJust
  putStrLn ""
  putStrLn "isProblemSolved"
  runTestTT testsIsProblemSolved
  putStrLn ""
  putStrLn "expandChainVariable"
  runTestTT testsExpandChainVariable
  putStrLn ""
  putStrLn "expandChainVariablesInBinds"
  runTestTT testsExpandChainVariablesInBinds
  putStrLn ""
 