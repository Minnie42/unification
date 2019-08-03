module Rules where
import Types
import Util

rule1 :: Sol -> Problem -> Maybe (Sol, Problem)
rule1 sol ((BL [B x y], BL [B x' y']):gamma) = Just (sol, ((V x, V x'):(V y, V y'):gamma))
rule1 sol _ = Nothing

rule2 :: Sol -> Problem -> Maybe (Sol, Problem)
rule2 sol ((V (Concrete x), V (Concrete y)):gamma)
	| x == y = Just (sol, gamma)
	| otherwise = Nothing
rule2 sol _ = Nothing	

rule3 :: Sol -> Problem -> Maybe (Sol, Problem)
rule3 sol ((V (Concrete x), V (Concrete y)):gamma)
	| x /= y = error "Fail"
	| otherwise = Nothing
rule3 sol _ = Nothing

rule4 :: Sol -> Problem -> Maybe (Sol, Problem)
rule4 sol ((V (Meta x), V (Concrete y)):gamma) = 
	Just ((Meta x, Concrete y):sol, applySubstitutionToGamma (Meta x, Concrete y) gamma)
rule4 sol _ = Nothing


rule5 :: Sol -> Problem -> Maybe (Sol, Problem)
rule5 sol ((V (Meta x), V (Meta y)):gamma) = 
	Just ((Meta x, Meta y):sol, applySubstitutionToGamma (Meta x, Meta y) gamma)
rule5 sol _ = Nothing

{-
rule6 :: Sol -> Problem -> Maybe [(Sol, Problem)]
rule6 sol ((BL (bind:binds), BL (bind':binds')):gamma) = 
	Just
	m = length (bind:binds)
	k = length (bind':binds')
rule6 sol _ = Nothing
-}

rule7 :: Sol -> Problem -> Maybe (Sol, Problem)
rule7 sol ((BL (bind:binds), BL []):gamma) = error "Fail"
rule7 sol _ = Nothing

rule8 :: Sol -> Problem -> Maybe (Sol, Problem)
rule8 sol ((BL [], BL (bind:binds)):gamma) = error "Fail"
rule8 sol _ = Nothing

rule9 :: Sol -> Problem -> Maybe (Sol, Problem)
rule9 sol ((BL [], BL []):gamma) = Just (sol, gamma)
rule9 sol _ = Nothing


