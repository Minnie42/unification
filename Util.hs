module Util where
import Types

maxChainVariableExpandSize :: Int
maxChainVariableExpandSize = 3


applySubstitutionToMeta :: SolEntry -> Var -> Var
applySubstitutionToMeta (Sub (Meta x) subVar) (Meta var)
  | var == x = subVar
  | otherwise = Meta var
applySubstitutionToMeta _ var = var

applySubstitutionToBind :: SolEntry -> Bind -> [Bind]
applySubstitutionToBind (Sub subVar1 subVar2) (B var1 var2) = 
  [B (applySubstitutionToMeta (Sub subVar1 subVar2) var1) (applySubstitutionToMeta (Sub subVar1 subVar2) var2)]
applySubstitutionToBind (Sub subVar1 subVar2) (CV name var1 var2) =
  [CV name (applySubstitutionToMeta (Sub subVar1 subVar2) var1) (applySubstitutionToMeta (Sub subVar1 subVar2) var2)]
applySubstitutionToBind (Exp expName expSize) (CV name startVar endVar)
  | expName == name = expandChainVariable (CV name startVar endVar) expSize
  | otherwise = [CV name startVar endVar]
applySubstitutionToBind _ bind = [bind]

applySubstitutionToSide :: SolEntry -> Side -> Side
applySubstitutionToSide substitution (V var) = V (applySubstitutionToMeta substitution var)
applySubstitutionToSide substitution (BL binds) = BL (concat (map (applySubstitutionToBind substitution) binds))

applySubstitutionToEquation :: SolEntry -> Equation -> Equation
applySubstitutionToEquation substitution (side1, side2) = 
  (applySubstitutionToSide substitution side1, applySubstitutionToSide substitution side2)

applySubstitutionToGamma :: SolEntry -> Problem -> Problem
applySubstitutionToGamma substitution gamma = map (applySubstitutionToEquation substitution) gamma

applySolutionToGamma :: Sol -> Problem -> Problem
applySolutionToGamma [] gamma = gamma
applySolutionToGamma sol gamma = applySolutionToGamma (init sol) (applySubstitutionToGamma (last sol) gamma)

unJust :: (Maybe a) -> a
unJust (Just value) = value

isEquationSolved :: Equation -> Bool
isEquationSolved (side1, side2) = side1 == side2

isProblemSolved :: Problem -> Bool
isProblemSolved problem = foldl (&&) True (map isEquationSolved problem)

expandChainVariable :: Bind -> Int -> [Bind]
expandChainVariable cv expandSize = reverse (expandChainVariableReverse cv expandSize)

expandChainVariableReverse :: Bind -> Int -> [Bind]
expandChainVariableReverse (CV _ startVar endVar) 1 = [B startVar endVar]
expandChainVariableReverse (CV name startVar endVar) expandSize = 
  (B newEndVar endVar)
  :(expandChainVariableReverse (CV name startVar newEndVar) (expandSize - 1))
  where
    newEndVar = Meta ("CV" ++ name ++ (show (expandSize - 1)))