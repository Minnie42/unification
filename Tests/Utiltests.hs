import Test.HUnit
import Test.Stats
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
      (applySubstitutionToMeta (Sub testMetaX testConcreteX) testMetaX)
  )

substituteMetaToMeta =
  TestCase (
    assertEqual
      "Substitute a meta variable with a substitution that starts with this meta variable and ends\
      \ with another meta variable should return the second meta variable."
      testMetaY
      (applySubstitutionToMeta (Sub testMetaX testMetaY) testMetaX)
  )

twoDifferentMetasNoSubstitution =
  TestCase (
    assertEqual
      "If the left side of the substitution doesn't match the variable no substitution should happen."
      testMetaY
      (applySubstitutionToMeta (Sub testMetaX testVar) testMetaY)
  )

concreteNoSubstitution =
  TestCase (
    assertEqual
      "If we get a concrete as variable to be substituted no substitution should happen."
      testConcreteX
      (applySubstitutionToMeta (Sub testMeta testVar) testConcreteX)
  )

noSubstitutionWithExpansion =
  TestCase (
    assertEqual
      "Given an expansion no substitution should happen."
      testVarX
      (applySubstitutionToMeta (Exp "test" 3) testVarX)
  )

-----------------------------------
-- applySolEntryToBind --------
-----------------------------------
substitutionInBind =
  TestCase (
    assertEqual
      "Given a Substitution that applies in the given bind the substitution should apply."
      [B testConcreteY testMetaY]
      (applySolEntryToBind (Sub testMetaX testConcreteY) (B testMetaX testMetaY))
  )
  
noSubstitutionInBind =
  TestCase (
    assertEqual
      "Given a Substitution that doesn't apply in the given bind no substitution should apply."
      [B testMetaY testMetaY]
      (applySolEntryToBind (Sub testMetaX testConcreteY) (B testMetaY testMetaY))
  )

substitutionInChainVariable =
  TestCase (
    assertEqual
      "Given a Substitution that applies in the given chain variable the substitution should apply."
      [CV "test" testConcreteY testMetaY]
      (applySolEntryToBind (Sub testMetaX testConcreteY) (CV "test" testMetaX testMetaY))
  )

expansionInChainVariable =
  TestCase (
    assertEqual
      "Given an expantion that matches the given chain variable the expansion should happen."
      [B testMetaX (Meta "CVtest1"), B (Meta "CVtest1") (Meta "CVtest2"), B (Meta "CVtest2") testMetaY]
      (applySolEntryToBind (Exp "test" 3) (CV "test" testMetaX testMetaY))
  )

noExpansionInChainVariable =
  TestCase (
    assertEqual
      "Given an expantion that does not matches the given chain variable the expansion should not happen."
      [CV "test2" testMetaX testMetaY]
      (applySolEntryToBind (Exp "test1" 3) (CV "test2" testMetaX testMetaY))
  )

-----------------------------------
-- applySolEntryToSide --------
-----------------------------------
substitutionInVariable =
  TestCase (
    assertEqual
      "Given a Substitution that applies in the given side that is a variabe the substitution should apply."
      (V testConcreteY)
      (applySolEntryToSide (Sub testMetaX testConcreteY) (V testMetaX))
  )

noSubstitutionInVariable =
  TestCase (
    assertEqual
      "Given a Substitution that doesn't apply in the given side that is a variabe no substitution should apply."
      (V testMetaY)
      (applySolEntryToSide (Sub testMetaX testConcreteY) (V testMetaY))
  )

substitutionInBinds =
  TestCase (
    assertEqual
      "Given a Substitution that applies in the given side that is a bind of size two the substitution should apply."
      (BL [(B testConcreteY testMetaY), (B testMetaY testConcreteY)])
      (applySolEntryToSide (Sub testMetaX testConcreteY) (BL [(B testMetaX testMetaY), (B testMetaY testMetaX)]))
  )

noSubstitutionInBinds =
  TestCase (
    assertEqual
      "Given a Substitution that doesn't apply in the given side that is a bind no substitution should apply."
      (BL [(B testMetaY testMetaY), (B testMetaY testMetaY)])
      (applySolEntryToSide (Sub testMetaX testConcreteY) (BL [(B testMetaY testMetaY), (B testMetaY testMetaY)]))
  )
  
noSubstitutionWithEmptySet =
  TestCase (
    assertEqual
      "Given an empty set no substitution should apply."
      (BL [])
      (applySolEntryToSide (Sub testMetaX testConcreteY) (BL []))
  )
  
-----------------------------------
-- applySolEntryToEquation ----
-----------------------------------
substitutionInEquation =
  TestCase (
    assertEqual
      "Given a Substitution that applies in the given equation the substitution should apply."
      (BL [B testConcreteY testMetaY], BL [B testMetaY testConcreteY])
      (applySolEntryToEquation (Sub testMetaX testConcreteY) (BL [B testMetaX testMetaY], BL [B testMetaY testMetaX]))
  )
  
noSubstitutionInEquation =
  TestCase (
    assertEqual
      "Given a Substitution that doesn't apply in the given equation no substitution should apply."
      (BL [B testMetaY testMetaY], BL [B testMetaY testMetaY])
      (applySolEntryToEquation (Sub testMetaX testConcreteY) (BL [B testMetaY testMetaY], BL [B testMetaY testMetaY]))
  )

-----------------------------------	
-- applySolEntryToGamma -----------
-----------------------------------
substitutionInGamma =
  TestCase (
    assertEqual
      "Given a Substitution that applies in the given gamma the substitution should apply."
      [(BL [B testConcreteY testMetaY], BL [B testMetaY testConcreteY]), (V testConcreteY, V testVarX)]
      (
       applySolEntryToGamma 
          (Sub testMetaX testConcreteY) 
          [(BL [B testMetaX testMetaY], BL [B testMetaY testMetaX]), (V testMetaX, V testVarX)]
      )
  )
  
noSubstitutionInGamma =
  TestCase (
    assertEqual
      "Given a Substitution that doesn't apply in the given gamma no substitution should apply."
      [(BL [B testMetaY testMetaY], BL [B testMetaY testMetaY]), (V testMetaY, V testVarX)]
      (
       applySolEntryToGamma 
          (Sub testMetaX testConcreteY) 
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
          [(Sub testMetaX testConcreteX), (Sub testMetaY testConcreteY)] 
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
          [(Sub testMetaY testConcreteY), (Sub testMetaX testMetaY)] 
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

-------------------------------
-- Tests ----------------------
-------------------------------
testsApplySubstitutionToMeta = 
  TestList [
    TestLabel "applySubstitutionToMeta_test1" substituteMetaToConcrete
    , TestLabel "applySubstitutionToMeta_test2" substituteMetaToMeta
    , TestLabel "applySubstitutionToMeta_test3" twoDifferentMetasNoSubstitution
    , TestLabel "applySubstitutionToMeta_test4" concreteNoSubstitution
    , TestLabel "applySubstitutionToMeta_test5" noSubstitutionWithExpansion
  ]

testsapplySolEntryToBind =
  TestList [
    TestLabel "applySolEntryToBind_test1" substitutionInBind
    , TestLabel "applySolEntryToBind_test2" noSubstitutionInBind
    , TestLabel "applySolEntryToBind_test3" substitutionInChainVariable
    , TestLabel "applySolEntryToBind_test4" expansionInChainVariable
    , TestLabel "applySolEntryToBind_test5" noExpansionInChainVariable
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

main = combineTests [
  (testsApplySubstitutionToMeta, "applySubstitutionToMeta")
  , (testsapplySolEntryToBind, "applySolEntryToBind")
  , (testsApplySubstitutionToSide, "applySolEntryToSide")
  , (testsApplySubstitutionToEquation, "applySolEntryToEquation")
  , (testsApplySubstitutionToGamma,  "applySolEntryToGamma")
  , (testsApplySolutionToGamma, "applySolutionToGamma")
  , (testsUnJust, "unJust")
  , (testsIsProblemSolved, "isProblemSolved")
  , (testsExpandChainVariable, "expandChainVariable")
  ]
 