import Test.HUnit
import Types
import Rules
import Util

-----------------------------------
-- applySubstitutionToMeta --------
-----------------------------------
applySubstitutionToMeta1 =
    TestCase (
        assertEqual
			"variable is in substitution so it gets substituted"
			(Concrete "y")
			(applySubstitutionToMeta (Meta "X", Concrete "y") (Meta "X"))
	)

applySubstitutionToMeta2 =
    TestCase (
        assertEqual
			"variable not in substitution so it returns variable"
			(Meta "Y")
			(applySubstitutionToMeta (Meta "X", Concrete "y") (Meta "Y"))
	)

-----------------------------------
-- applySubstitutionToBind --------
-----------------------------------

-----------------------------------
-- applySubstitutionToSide --------
-----------------------------------

-----------------------------------
-- applySubstitutionToEquation ----
-----------------------------------

-----------------------------------	
-- applySubstitutionToGamma -------
-----------------------------------

testUtils = 
	TestList [
		TestLabel "applySubstitutionToMeta_test1" applySubstitutionToMeta1
		, TestLabel "applySubstitutionToMeta_test1" applySubstitutionToMeta2
		--, TestLabel "utils_test3" ???
	]
	
main = do 
	runTestTT testUtils