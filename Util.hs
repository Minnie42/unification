module Util where
import Types

maxChainVariableExpandSize :: Int
maxChainVariableExpandSize = 3


applySubstitutionToMeta :: SolEntry -> Var -> Var
applySubstitutionToMeta (Sub (Meta x) subVar) (Meta var)
  | var == x = subVar
  | otherwise = Meta var
applySubstitutionToMeta _ var = var

applySolEntryToBind :: SolEntry -> Bind -> [Bind]
applySolEntryToBind (Sub subVar1 subVar2) (B var1 var2) = 
  [B (applySubstitutionToMeta (Sub subVar1 subVar2) var1) (applySubstitutionToMeta (Sub subVar1 subVar2) var2)]
applySolEntryToBind (Sub subVar1 subVar2) (CV name var1 var2) =
  [CV name (applySubstitutionToMeta (Sub subVar1 subVar2) var1) (applySubstitutionToMeta (Sub subVar1 subVar2) var2)]
applySolEntryToBind (Exp expName expSize) (CV name startVar endVar)
  | expName == name = expandChainVariable (CV name startVar endVar) expSize
  | otherwise = [CV name startVar endVar]
applySolEntryToBind _ bind = [bind]

applySolEntryToSide :: SolEntry -> Side -> Side
applySolEntryToSide substitution (V var) = V (applySubstitutionToMeta substitution var)
applySolEntryToSide substitution (BL binds) = BL (concat (map (applySolEntryToBind substitution) binds))

applySolEntryToEquation :: SolEntry -> Equation -> Equation
applySolEntryToEquation substitution (side1, side2) = 
  (applySolEntryToSide
 substitution side1, applySolEntryToSide
 substitution side2)

applySolEntryToGamma :: SolEntry -> Problem -> Problem
applySolEntryToGamma substitution gamma = map (applySolEntryToEquation substitution) gamma

applySolutionToGamma :: Sol -> Problem -> Problem
applySolutionToGamma [] gamma = gamma
applySolutionToGamma sol gamma = applySolutionToGamma (init sol) (applySolEntryToGamma (last sol) gamma)

unJust :: (Maybe a) -> a
unJust (Just value) = value

isEquationSolved :: Equation -> Bool
isEquationSolved (side1, side2) = side1 == side2

isProblemSolved :: Problem -> Bool
isProblemSolved problem = foldl (&&) True (map isEquationSolved problem)

expandChainVariable :: Bind -> Int -> [Bind]
expandChainVariable cv expandSize = 
  reverse (expandChainVariableReverse cv expandSize)

expandChainVariableReverse :: Bind -> Int -> [Bind]
expandChainVariableReverse (CV _ startVar endVar) 1 = 
  [B startVar endVar]
expandChainVariableReverse 
  (CV name startVar endVar) 
  expandSize = 
  (B newEndVar endVar)
  :(
    expandChainVariableReverse 
      (CV name startVar newEndVar) 
      (expandSize - 1)
  )
  where
    newEndVar = 
      Meta ("CV" ++ name ++ (show (expandSize - 1)))

compareEquations :: Equation -> Equation -> Ordering
compareEquations eq1 eq2 = prio eq1 `compare` prio eq2 
  where
    prio (BL [], _) = (-2, 0)
    prio (_, BL []) = (-2, 0)
    prio (V _, _) = (-1, 0)
    prio (_, V _) = (-1, 0)
    prio (BL binds1, BL binds2) = (countChainVariablesInEquation (BL binds1, BL binds2), (length binds1) + (length binds2))

countChainVariablesInBind :: [Bind] -> Integer
countChainVariablesInBind binds = sum (map (\bind -> if isChainVariable bind then 1 else 0) binds)

countChainVariablesInEquation :: Equation -> Integer
countChainVariablesInEquation (BL bind1, BL bind2) = (countChainVariablesInBind bind1) + (countChainVariablesInBind bind2)
countChainVariablesInEquation (BL bind, _) = countChainVariablesInBind bind
countChainVariablesInEquation (_, BL bind) = countChainVariablesInBind bind
countChainVariablesInEquation _ = 0

isChainVariable :: Bind -> Bool
isChainVariable (CV _ _ _) = True
isChainVariable _ = False