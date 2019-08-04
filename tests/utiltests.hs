import Test.HUnit
import Types
import Util

-----------------------------------
-- applySubstitutionToMeta --------
-----------------------------------
applySubstitutionToMeta1 =
  TestCase (
    assertEqual
      "variable is in substitution so it gets substituted"
      (Concrete "y")
      (applySubstitutionToMeta sub var)
  )
  where
    sub = (Meta "X", Concrete "y")
    var = Meta "X"

applySubstitutionToMeta2 =
  TestCase (
    assertEqual
      "variable not in substitution so it returns variable"
      var
      (applySubstitutionToMeta sub var)
  )
  where
    sub = (Meta "X", Concrete "y")
    var = Meta "Y"

applySubstitutionToMeta3 =
  TestCase (
    assertEqual
      "variable is in substitution so it gets substituted"
      (Meta "Y")
      (applySubstitutionToMeta sub var)
  )
  where
    sub = (Meta "X", Meta "Y")
    var = Meta "X"

applySubstitutionToMeta4 =
  TestCase (
    assertEqual
      "variable not in substitution so it returns variable"
      var
      (applySubstitutionToMeta sub var)
  )
  where
    sub = (Meta "X", Meta "Y")
    var = Concrete "x"

-----------------------------------
-- applySubstitutionToBind --------
-----------------------------------
applySubstitutionToBind1 =
  TestCase (
    assertEqual
      "one variable in bind is in substitution so it gets substituted"
      (B (Concrete "x") (Concrete "y"))
      (applySubstitutionToBind sub bind)
  )
  where
    sub = (Meta "X", Concrete "y")
    bind = (B (Concrete "x") (Meta "X"))

applySubstitutionToBind2 =
  TestCase (
    assertEqual
      "no variable in bind is in substitution so it reurns bind"
      (B (Concrete "x") (Meta "Y"))
      (applySubstitutionToBind sub bind)
  )
  where
    sub = (Meta "X", Concrete "y")
    bind = (B (Concrete "x") (Meta "Y"))

-----------------------------------
-- applySubstitutionToSide --------
-----------------------------------
applySubstitutionToSide1 =
  TestCase (
    assertEqual
      "?" --TODO: comment
      (BL [B (Concrete "y") (Meta "Y")])
      (applySubstitutionToSide sub side)
  )
  where
    sub = (Meta "X", Concrete "y")
    side = BL [B (Meta "X") (Meta "Y")]

applySubstitutionToSide2 =
  TestCase (
    assertEqual
      "?" --TODO: comment
      (BL [B (Meta "Y") (Concrete "y")])
      (applySubstitutionToSide sub side)
  )
  where
    sub = (Meta "X", Meta "Y")
    side = BL [B (Meta "X") (Concrete "y")]

applySubstitutionToSide3 =
  TestCase (
    assertEqual
      "?" --TODO: comment
      (BL [B (Concrete "y") (Concrete "x")])
      (applySubstitutionToSide sub side)
  )
  where
    sub = (Meta "X", Concrete "y")
    side = BL [B (Concrete "y") (Concrete "x")]

-----------------------------------
-- applySubstitutionToEquation ----
-----------------------------------
applySubstitutionToEquation1 =
  TestCase (
    assertEqual
      "" --TODO
      (V (Meta "Y"), V (Concrete "y"))
      (applySubstitutionToEquation sub equation)
  )
  where
    sub = (Meta "X", Meta "Y")
    equation = (V (Meta "X"), V (Concrete "y"))
      
applySubstitutionToEquation2 =
  TestCase (
    assertEqual
      "" --TODO
      (BL [B (Concrete "x") (Concrete "y")], (V (Concrete "y")))
      (applySubstitutionToEquation sub equation)
  )
  where
    sub = (Meta "X", Concrete "x")
    equation = ((BL [B (Meta "X") (Concrete "y")]), (V (Concrete "y")))

applySubstitutionToEquation3 =
  TestCase (
    assertEqual
      "" --TODO
      ((BL [B (Meta "Y") (Concrete "y")]), (V (Concrete "y")))
      (applySubstitutionToEquation sub equation)
  )
  where
    sub = (Meta "X", Concrete "x")
    equation = ((BL [B (Meta "Y") (Concrete "y")]), (V (Concrete "y")))

-----------------------------------	
-- applySubstitutionToGamma -------
-----------------------------------
applySubstitutionToGamma1 =
  TestCase (
    assertEqual
      "" --TODO
      ([(V (Concrete "x") ,V (Concrete "x"))])
      (applySubstitutionToGamma sub gamma)
  )
  where
    sub = (Meta "X", Concrete "x")
    gamma = [(V (Concrete "x") ,V (Meta "X"))]
    
applySubstitutionToGamma2 =
  TestCase (
    assertEqual
      "" --TODO
      ([(V (Concrete "x") ,V (Meta "Y"))])
      (applySubstitutionToGamma sub gamma)
  )
  where
    sub = (Meta "X", Meta "Y")
    gamma = [(V (Concrete "x") ,V (Meta "X"))]
  
applySubstitutionToGamma3 =
  TestCase (
    assertEqual
      "" --TODO
      ([(V (Concrete "x") ,V (Meta "Y"))])
      (applySubstitutionToGamma sub gamma)
  )
  where
    sub = (Meta "X", Meta "Y")
    gamma = [(V (Concrete "x") ,V (Meta "Y"))]

-----------------------------------
-- unJust -------------------------
-----------------------------------
unJust1 =
  TestCase (
    assertEqual
      ""--TODO
      (value)
      (unJust (Just value))
  )
  where
    value = 10

-------------------------------
-- Tests ----------------------
-------------------------------
testsApplySubstitutionToMeta = 
  TestList [
    TestLabel "applySubstitutionToMeta_test1" applySubstitutionToMeta1
    , TestLabel "applySubstitutionToMeta_test2" applySubstitutionToMeta2
    , TestLabel "applySubstitutionToMeta_test3" applySubstitutionToMeta3
    , TestLabel "applySubstitutionToMeta_test4" applySubstitutionToMeta4
  ]

testsApplySubstitutionToBind =
  TestList [
    TestLabel "applySubstitutionToBind_test1" applySubstitutionToBind1
    , TestLabel "applySubstitutionToBind_test2" applySubstitutionToBind2
  ]

testsApplySubstitutionToSide =
  TestList [
    TestLabel "applySubstitutionToSide_test1" applySubstitutionToSide1
    , TestLabel "applySubstitutionToSide_test2" applySubstitutionToSide2
    , TestLabel "applySubstitutionToSide_test3" applySubstitutionToSide3
  ]

testsApplySubstitutionToEquation =
  TestList [
    TestLabel "applySubstitutionToEquation_test1" applySubstitutionToEquation1
    , TestLabel "applySubstitutionToEquation_test2" applySubstitutionToEquation2
    , TestLabel "applySubstitutionToEquation_test3" applySubstitutionToEquation3
  ]

testsApplySubstitutionToGamma =
  TestList [
    TestLabel "applySubstitutionToGamma_test1" applySubstitutionToGamma1
    , TestLabel "applySubstitutionToGamma_test2" applySubstitutionToGamma2
    , TestLabel "applySubstitutionToGamma_test3" applySubstitutionToGamma3
  ]

testsUnJust =
  TestList [
    TestLabel "unJust_test1" unJust1
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
  putStrLn "unJust"
  runTestTT testsUnJust
  putStrLn ""