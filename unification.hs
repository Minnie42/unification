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
  | next == Nothing = recursiveUnification ((sol, problem):problems) sols startRule
  | otherwise = recursiveUnification ((unJust next):problems) sols 7
  where
    next = rule3 sol problem
recursiveUnification ((sol, problem):problems) sols 7
  | next == Nothing = recursiveUnification ((sol, problem):problems) sols startRule
  | otherwise = recursiveUnification ((unJust next):problems) sols 8
  where
    next = rule7 sol problem      
recursiveUnification ((sol, problem):problems) sols 8
  | next == Nothing = recursiveUnification ((sol, problem):problems) sols startRule
  | otherwise = recursiveUnification ((unJust next):problems) sols 1
  where
    next = rule8 sol problem
recursiveUnification ((sol, problem):problems) sols 2
  | next == Nothing = recursiveUnification ((sol, problem):problems) sols startRule
  | otherwise = recursiveUnification ((unJust next):problems) sols 9
  where
    next = rule2 sol problem
recursiveUnification ((sol, problem):problems) sols 9
  | next == Nothing = recursiveUnification ((sol, problem):problems) sols startRule
  | otherwise = recursiveUnification ((unJust next):problems) sols 1
  where
    next = rule9 sol problem
recursiveUnification ((sol, problem):problems) sols 1
  | next == Nothing = recursiveUnification ((sol, problem):problems) sols startRule
  | otherwise = recursiveUnification ((unJust next):problems) sols 4
  where
    next = rule1 sol problem   
recursiveUnification ((sol, problem):problems) sols 4
  | next == Nothing = recursiveUnification ((sol, problem):problems) sols startRule
  | otherwise = recursiveUnification ((unJust next):problems) sols 5
  where
    next = rule4 sol problem   
recursiveUnification ((sol, problem):problems) sols 5
  | next == Nothing = recursiveUnification ((sol, problem):problems) sols startRule
  | otherwise = recursiveUnification ((unJust next):problems) sols 6
  where
    next = rule5 sol problem      
recursiveUnification ((sol, problem):problems) sols 6
  | next == Nothing = recursiveUnification ((sol, problem):problems) sols startRule
  | otherwise = error "no rule matched, check your input"
  where
    next = rule6 sol problem

