import Unification
import Tests.DummyVariables
import Types
import System.Environment
import Util

main = do 
         benchmark:l:_ <- getArgs
         let n = (read l)::Int
         case benchmark of
            "solved" -> print (unification (solvedEquations n))
            "unsolved" -> print (unification (unsolvedEquations n))
            "bind" -> print (unification (bindProblem n))
            "cyc" -> print (unification (cyclicalProblem n))
            "unsolvedcyc" -> print (unification (unsolvableCyclicalProblem n))
            "opt" -> print (unification (testForOptimization n))
            _ -> putStrLn "no benchmark specified"
        

solvedEquations :: Int -> Problem
solvedEquations 0 = []
solvedEquations n = (V testVarX, V testVarX):(solvedEquations (n - 1))

unsolvedEquations :: Int -> Problem
unsolvedEquations 0 = []
unsolvedEquations n = (V (Meta ("A" ++ (show n))),V (Meta ("B" ++ (show n)))):(unsolvedEquations (n - 1))

bindProblem :: Int -> Problem
bindProblem n = [(BL (growingBind n "a" "b"), BL (growingBind n "c" "d"))]

growingBind :: Int -> String -> String -> [Bind]
growingBind 0 _ _ = []
growingBind n a b = (B (Meta (a ++ (show n))) (Meta (b ++ (show n)))):(growingBind (n - 1) a b)
 
bindProblemWithCV :: Int -> Problem
bindProblemWithCV n = [(BL (growingBindWithCV n "a" "b" "s"), BL (growingBindWithCV n "c" "d" "t"))]

growingBindWithCV :: Int -> String -> String -> String -> [Bind]
growingBindWithCV 0 _ _ _ = []
growingBindWithCV n a b s = (CV (s ++ (show n)) (Meta (a ++ (show n))) (Meta (b ++ (show n)))):(growingBindWithCV (n - 1) a b s)

bindProblemWithOneCV :: Int -> Problem
bindProblemWithOneCV 0 = []
bindProblemWithOneCV n = [(BL ((growingBindWithCV 1 "a" "b" "s") ++ (growingBind (n - 1) "c" "d")), BL ((growingBindWithCV 1 "e" "f" "t") ++ (growingBind (n - 1) "g" "h")))]

cyclicalProblem :: Int -> Problem
cyclicalProblem n = conv (expandChainVariable (CV "s" testMetaX testMetaX) n)
  where 
    conv ((B var1 var2):binds) = (V var1, V var2):(conv binds)
    conv [] = []

unsolvableCyclicalProblem :: Int -> Problem
unsolvableCyclicalProblem n = (cyclicalProblem n) ++ [(V testMetaX, V testConcreteX), (V (Meta ("CVs" ++ (show (n-1)))), V testConcreteY)]


testForOptimization :: Int -> Problem 
testForOptimization n = (bindProblem n) ++ [(V testConcreteX, V testConcreteY)]