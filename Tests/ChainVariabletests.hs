import Test.HUnit
import Test.Stats
import Types
import Unification
import Tests.DummyVariables

twoCVsOnTheLeftOneOnTheRight =
    TestCase (
      assertEqual
        "Combining multiple chain variables should have plenty of solutions."
        [
          [Sub testMetaZ (Meta "CVU1"), Sub testMetaW testMetaA, Exp "T" 1, Sub testMetaY (Meta "CVU2"), Sub testMetaB (Meta "CVU1"), Sub (Meta "CVS1") testMetaB, Sub testMetaX (Meta "CVU2"), Exp "U" 3, Exp "S" 2]
        , [Sub testMetaZ (Meta "CVU2"), Sub testMetaW (Meta "CVU1"), Exp "T" 1, Sub testMetaY (Meta "CVU1"), Sub testMetaB testMetaA, Sub (Meta "CVS1") testMetaB, Sub testMetaX (Meta "CVU2"), Exp "U" 3, Exp "S" 2]
        , [Sub testMetaZ (Meta "CVU1"), Sub testMetaW testMetaA, Exp "T" 1, Sub testMetaY testMetaB, Sub (Meta "CVS1") (Meta "CVU2"), Sub testMetaX (Meta "CVU1"), Exp "U" 3, Exp "S" 2]
        , [Sub testMetaZ testMetaB, Sub testMetaW testMetaA, Exp "T" 1, Sub testMetaY (Meta "CVU1"), Sub (Meta "CVU2") testMetaA, Sub (Meta "CVS1") (Meta "CVU2"), Sub testMetaX (Meta "CVU1"), Exp "U" 3, Exp "S" 2]
        , [Sub testMetaZ (Meta "CVU2"), Sub testMetaW (Meta "CVU2"), Exp "T" 1, Sub testMetaY testMetaB, Sub (Meta "CVU1") (Meta "CVU2"), Sub (Meta "CVS1") (Meta "CVU1"), Sub testMetaX testMetaA, Exp "U" 3, Exp "S" 2]
        , [Sub testMetaZ testMetaB, Sub testMetaW (Meta "CVU2"), Exp "T" 1, Sub testMetaY (Meta "CVU2"), Sub (Meta "CVS1") (Meta "CVU1"), Sub testMetaX testMetaA, Exp "U" 3, Exp "S" 2]
        , [Sub testMetaZ (Meta "CVU1"), Sub (Meta "CVU2") testMetaA, Sub (Meta "CVT1") (Meta "CVU2"), Sub testMetaW (Meta "CVU1"),Exp "T" 2, Sub testMetaY testMetaB,Sub testMetaX (Meta "CVU2"), Exp "U" 3, Exp "S" 1]
        , [Sub testMetaZ (Meta "CVU2"), Sub (Meta "CVT1") (Meta "CVU1"), Sub testMetaW testMetaA, Exp "T" 2, Sub testMetaY testMetaB,Sub testMetaX (Meta "CVU2"), Exp "U" 3, Exp "S" 1]
        , [Sub testMetaZ (Meta "CVU1"), Sub testMetaB testMetaA, Sub (Meta "CVT1") testMetaB, Sub testMetaW (Meta "CVU2"), Exp "T" 2, Sub testMetaY (Meta "CVU2"),Sub testMetaX (Meta "CVU1"), Exp "U" 3, Exp "S" 1]
        , [Sub testMetaZ testMetaB, Sub (Meta "CVU1") (Meta "CVU2"), Sub (Meta "CVT1") (Meta "CVU1"), Sub testMetaW testMetaA, Exp "T" 2, Sub testMetaY (Meta "CVU2"),Sub testMetaX (Meta "CVU1"), Exp "U" 3, Exp "S" 1]
        , [Sub testMetaZ (Meta "CVU2"), Sub testMetaB (Meta "CVU1"), Sub (Meta "CVT1") testMetaB, Sub testMetaW (Meta "CVU2"), Exp "T" 2, Sub testMetaY (Meta "CVU1"),Sub testMetaX testMetaA, Exp "U" 3, Exp "S" 1]
        , [Sub testMetaZ testMetaB, Sub (Meta "CVT1") (Meta "CVU2"), Sub testMetaW (Meta "CVU1"),Exp "T" 2, Sub testMetaY (Meta "CVU1"),Sub testMetaX testMetaA, Exp "U" 3, Exp "S" 1]
        , [Sub testMetaZ (Meta "CVU1"), Sub testMetaW testMetaA, Exp "T" 1, Sub testMetaY testMetaB, Sub testMetaX (Meta "CVU1"), Exp "U" 2, Exp "S" 1]
        , [Sub testMetaZ testMetaB, Sub testMetaW (Meta "CVU1"), Exp "T" 1, Sub testMetaY (Meta "CVU1"), Sub testMetaX testMetaA, Exp "U" 2, Exp "S" 1]
        ]
        (
          unification 
            [(BL [CV "S" testMetaX testMetaY, CV "T" testMetaW testMetaZ], BL [CV "U" testMetaA testMetaB])]
        )
    )

  
fourCVsOnTheLeftOneOnTheRight =
  TestCase (
    assertEqual
      "If the difference in size of the equations is bigger then set limit for chain variable\
      \ expansion we should get no solution."
      []
      (
        unification 
          [(BL [CV "S" testMetaX testMetaY, CV "T" testMetaW testMetaZ, CV "U" testMetaA testMetaB, CV "V" testMetaW testMetaY], BL [CV "W" testMetaA testMetaB])]
      )
  )  

sevenCVsOnTheLeftTwoOnTheRight =
  TestCase (
    assertEqual
      "If the difference in size of the equations is bigger then set limit for chain variable\
      \ expansion we should get no solution."
      []
      (
        unification 
          [(BL [CV "R" testMetaY testMetaW, CV "S" testMetaX testMetaY, CV "T" testMetaW testMetaZ, CV "U" testMetaA testMetaB, CV "V" testMetaW testMetaY, CV "V" testMetaW testMetaZ, CV "V" testMetaX testMetaY], BL [CV "Y" testMetaX testMetaW, CV "Z" testMetaA testMetaB])]
      )
  )

sixCVsOnTheLeftTwoOnTheRight =
  TestCase (
    assertEqual
      "Combining multiple chain variables should have plenty of solutions."
      True
      ((length (
        unification 
          [(BL [CV "S" testMetaX testMetaY, CV "T" testMetaW testMetaZ, CV "U" testMetaA testMetaB, CV "V" testMetaW testMetaY, CV "V" testMetaW testMetaZ, CV "V" testMetaX testMetaY], BL [CV "Y" testMetaX testMetaW, CV "Z" testMetaA testMetaB])]
      )) > 0)
  )

{-
sixteenCVsOnTheLeftFiveOnTheRight =
  TestCase (
    assertEqual
      "If the difference in size of the equations is bigger then set limit for chain variable\
      \ expansion we should get no solution."
      []
      (
        unification 
          [(BL [CV "F" testMetaY testMetaW, CV "G" testMetaX testMetaY, CV "H" testMetaW testMetaZ, CV "I" testMetaA testMetaB, CV "J" testMetaW testMetaY, CV "K" testMetaW testMetaZ, CV "L" testMetaX testMetaY, CV "M" testMetaY testMetaW, CV "N" testMetaX testMetaY, CV "O" testMetaW testMetaZ, CV "P" testMetaA testMetaB, CV "Q" testMetaW testMetaY, CV "R" testMetaW testMetaZ, CV "S" testMetaX testMetaY, CV "T" testMetaW testMetaZ, CV "U" testMetaX testMetaY], BL [CV "V" testMetaX testMetaW, CV "W" testMetaA testMetaB, CV "X" testMetaW testMetaZ, CV "Y" testMetaX testMetaY, CV "Z" testMetaX testMetaY])]
      )
  )
-}
testsChainVariables =
  TestList [
    TestLabel "chainVariables_test1" twoCVsOnTheLeftOneOnTheRight
    , TestLabel "chainVariables_test2" fourCVsOnTheLeftOneOnTheRight
    , TestLabel "chainVariables_test3" sevenCVsOnTheLeftTwoOnTheRight
    --, TestLabel "chainVariables_test4" sixteenCVsOnTheLeftFiveOnTheRight
    , TestLabel "chainVariables_test5" sixCVsOnTheLeftTwoOnTheRight
  ]

main = combineTests [
  (testsChainVariables, "chainVariables")
  ]