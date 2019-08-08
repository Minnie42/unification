module Rules where
import Types
import Util

rule1 :: Sol -> Problem -> Maybe (Maybe Sol, Problem)
rule1 sol ((BL [B x y], BL [B x' y']):gamma) = Just (Just sol, ((V x, V x'):(V y, V y'):gamma))
rule1 sol _ = Nothing

rule2 :: Sol -> Problem -> Maybe (Maybe Sol, Problem)
rule2 sol ((V (x), V (y)):gamma)
  | x == y = Just (Just sol, gamma)
  | otherwise = Nothing
rule2 sol _ = Nothing

rule3 :: Sol -> Problem -> Maybe (Maybe Sol, Problem)
rule3 sol ((V (Concrete x), V (Concrete y)):gamma)
  | x /= y = Just (Nothing, (V (Concrete x), V (Concrete y)):gamma)
  | otherwise = Nothing
rule3 sol _ = Nothing

rule4 :: Sol -> Problem -> Maybe (Maybe Sol, Problem)
rule4 sol ((V (Meta x), V (Concrete y)):gamma) = 
  Just (Just ((Meta x, Concrete y):sol), applySubstitutionToGamma (Meta x, Concrete y) ((V (Meta x), V (Concrete y)):gamma))
rule4 sol ((V (Concrete y), V (Meta x)):gamma) = 
  Just (Just ((Meta x, Concrete y):sol), applySubstitutionToGamma (Meta x, Concrete y) ((V (Meta x), V (Concrete y)):gamma))
rule4 sol _ = Nothing

rule5 :: Sol -> Problem -> Maybe (Maybe Sol, Problem)
rule5 sol ((V (Meta x), V (Meta y)):gamma) 
  |x /= y = Just (Just ((Meta x, Meta y):sol), applySubstitutionToGamma (Meta x, Meta y) ((V (Meta x), V (Meta y)):gamma))
  |otherwise = Nothing
rule5 sol _ = Nothing

rule6 :: Sol -> Problem -> Maybe [(Maybe Sol, Problem)]
rule6 sol ((BL (leftBind1:leftBind2:leftBinds), BL (rightBind:rightBinds)):gamma) = 
  Just (rule6Iterate (leftBind1:leftBind2:leftBinds) [] rightBind rightBinds sol gamma)
rule6 sol ((BL (leftBind:leftBinds), BL (rightBind1:rightBind2:rightBinds)):gamma) = 
  Just (rule6Iterate (leftBind:leftBinds) [] rightBind1 (rightBind2:rightBinds) sol gamma)
rule6 sol _ = Nothing

rule6Iterate :: [Bind] -> [Bind] -> Bind -> [Bind] -> Sol -> Problem -> [(Maybe Sol, Problem)]
rule6Iterate leftBind rightInit rightCurrent [] sol gamma = 
  (
    Just sol, 
    (BL [head leftBind], BL [rightCurrent])
    :(BL (tail leftBind), BL rightInit)
    :gamma
  ):[]
rule6Iterate leftBind rightInit rightCurrent (rightNext:rightTail) sol gamma = 
  (
    Just sol, 
    (BL [head leftBind], BL [rightCurrent])
    :(BL (tail leftBind), BL (rightInit ++ (rightNext:rightTail)))
    :gamma
  ):(rule6Iterate leftBind (rightInit ++ [rightCurrent]) rightNext rightTail sol gamma)

rule7 :: Sol -> Problem -> Maybe (Maybe Sol, Problem)
rule7 sol ((BL (bind:binds), BL []):gamma) = Just (Nothing, ((BL (bind:binds), BL []):gamma))
rule7 sol _ = Nothing

rule8 :: Sol -> Problem -> Maybe (Maybe Sol, Problem)
rule8 sol ((BL [], BL (bind:binds)):gamma) = Just (Nothing, ((BL [], BL (bind:binds)):gamma))
rule8 sol _ = Nothing

rule9 :: Sol -> Problem -> Maybe (Maybe Sol, Problem)
rule9 sol ((BL [], BL []):gamma) = Just (Just sol, gamma)
rule9 sol _ = Nothing


