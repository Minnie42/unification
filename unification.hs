module Unification where
import Rules
import Types
import Util

startRule :: Int
startRule = 3

unification  :: Problem -> [Sol]
unification gamma = recursiveUnification [((Just []), gamma)] [] startRule

recursiveUnification :: [(Maybe Sol, Problem)] -> [Sol] -> Int -> [Sol]
recursiveUnification ((Nothing, _):problems) sols _ = recursiveUnification problems sols startRule
recursiveUnification ((Just sol, []):problems) sols _ = recursiveUnification problems (sol:sols) startRule
recursiveUnification [] sols _ = sols

recursiveUnification ((sol, problem):problems) sols 3
  | next == Nothing = recursiveUnification ((sol, problem):problems) sols 7
  | otherwise = recursiveUnification ((unJust next):problems) sols startRule
  where
    next = rule3 sol problem
recursiveUnification ((sol, problem):problems) sols 7
  | next == Nothing = recursiveUnification ((sol, problem):problems) sols 8
  | otherwise = recursiveUnification ((unJust next):problems) sols startRule
  where
    next = rule7 sol problem      
recursiveUnification ((sol, problem):problems) sols 8
  | next == Nothing = recursiveUnification ((sol, problem):problems) sols 2
  | otherwise = recursiveUnification ((unJust next):problems) sols startRule
  where
    next = rule8 sol problem
recursiveUnification ((sol, problem):problems) sols 2
  | next == Nothing = recursiveUnification ((sol, problem):problems) sols 9
  | otherwise = recursiveUnification ((unJust next):problems) sols startRule
  where
    next = rule2 sol problem
recursiveUnification ((sol, problem):problems) sols 9
  | next == Nothing = recursiveUnification ((sol, problem):problems) sols 1
  | otherwise = recursiveUnification ((unJust next):problems) sols startRule
  where
    next = rule9 sol problem
recursiveUnification ((sol, problem):problems) sols 1
  | next == Nothing = recursiveUnification ((sol, problem):problems) sols 4
  | otherwise = recursiveUnification ((unJust next):problems) sols startRule
  where
    next = rule1 sol problem   
recursiveUnification ((sol, problem):problems) sols 4
  | next == Nothing = recursiveUnification ((sol, problem):problems) sols 5
  | otherwise = recursiveUnification ((unJust next):problems) sols startRule
  where
    next = rule4 sol problem   
recursiveUnification ((sol, problem):problems) sols 5
  | next == Nothing = recursiveUnification ((sol, problem):problems) sols 6
  | otherwise = recursiveUnification ((unJust next):problems) sols startRule
  where
    next = rule5 sol problem      
recursiveUnification ((sol, problem):problems) sols 6
  | next == Nothing = recursiveUnification ((sol, problem):problems) sols 10
  | otherwise = recursiveUnification ((unJust next) ++ problems) sols startRule
  where
    next = rule6 sol problem
recursiveUnification ((sol, problem):problems) sols 10
  | next == Nothing = recursiveUnification ((sol, problem):problems) sols 11
  | otherwise = recursiveUnification ((unJust next) ++ problems) sols startRule
  where
    next = rule10 sol problem
recursiveUnification ((sol, problem):problems) sols 11
  | next == Nothing = error "No rule applied. Check your input."
  | otherwise = recursiveUnification ((unJust next) ++ problems) sols startRule
  where
    next = rule11 sol problem