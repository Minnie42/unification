module Test.Stats where
import Test.HUnit

combineTests :: [(Test, String)] -> IO ()
combineTests tests = combineTestsIterate tests (Counts 0 0 0 0)

combineTestsIterate :: [(Test, String)] -> Counts -> IO ()
combineTestsIterate [] counts = print counts
combineTestsIterate ((test, title):tests) counts = 
  do
    putStrLn title
    currentCounts <- runTestTT test
    putStrLn ""
    combineTestsIterate tests (combineCounts counts currentCounts)
  
combineCounts :: Counts -> Counts -> Counts
combineCounts (Counts c1 t1 e1 f1) (Counts c2 t2 e2 f2) = Counts (c1 + c2) (t1 + t2) (e1 + e2) (f1 + f2)