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