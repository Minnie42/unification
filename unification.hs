module Unification where
import Rules
import Types
import Util

unification  :: Problem -> Maybe Sol
unification gamma = recursiveUnification (Just []) gamma 1

recursiveUnification :: Maybe Sol -> Problem -> Int -> Maybe Sol
recursiveUnification Nothing _ _ = Nothing
recursiveUnification sol [] _ = sol
recursiveUnification sol problem 1
  | next == Nothing = recursiveUnification sol problem 2
  | otherwise = recursiveUnification (fst (unJust next)) (snd (unJust next)) 2
  where
    next = rule1 sol problem
recursiveUnification sol problem 2
  | next == Nothing = recursiveUnification sol problem 3
  | otherwise = recursiveUnification (fst (unJust next)) (snd (unJust next)) 3
  where
    next = rule2 sol problem
recursiveUnification sol problem 3
  | next == Nothing = recursiveUnification sol problem 4
  | otherwise = recursiveUnification (fst (unJust next)) (snd (unJust next)) 4
  where
    next = rule3 sol problem
recursiveUnification sol problem 4
  | next == Nothing = recursiveUnification sol problem 5
  | otherwise = recursiveUnification (fst (unJust next)) (snd (unJust next)) 5
  where
    next = rule4 sol problem
recursiveUnification sol problem 5
  | next == Nothing = recursiveUnification sol problem 6
  | otherwise = recursiveUnification (fst (unJust next)) (snd (unJust next)) 6
  where
    next = rule5 sol problem
recursiveUnification sol problem 6
  | next == Nothing = recursiveUnification sol problem 7
  | otherwise = recursiveUnification (fst (head (unJust next))) (snd (head (unJust next))) 7
  where
    next = rule6 sol problem    
recursiveUnification sol problem 7
  | next == Nothing = recursiveUnification sol problem 8
  | otherwise = recursiveUnification (fst (unJust next)) (snd (unJust next)) 8
  where
    next = rule7 sol problem
recursiveUnification sol problem 8
  | next == Nothing = recursiveUnification sol problem 9
  | otherwise = recursiveUnification (fst (unJust next)) (snd (unJust next)) 9
  where
    next = rule8 sol problem
recursiveUnification sol problem 9
  | next == Nothing = recursiveUnification sol problem 1
  | otherwise = recursiveUnification (fst (unJust next)) (snd (unJust next)) 1
  where
    next = rule9 sol problem