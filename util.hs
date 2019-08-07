module Util where
import Types

applySubstitutionToMeta :: (Var, Var) -> Var -> Var
applySubstitutionToMeta (Meta x, subVar) (Meta var)
  | var == x = subVar
  | otherwise = Meta var
applySubstitutionToMeta _ var = var

applySubstitutionToBind :: (Var, Var) -> Bind -> Bind
applySubstitutionToBind substitution (B var1 var2) = 
  B (applySubstitutionToMeta substitution var1) (applySubstitutionToMeta substitution var2)

applySubstitutionToSide :: (Var, Var) -> Side -> Side
applySubstitutionToSide substitution (V var) = V (applySubstitutionToMeta substitution var)
applySubstitutionToSide substitution (BL binds) = BL (map (applySubstitutionToBind substitution) binds)

applySubstitutionToEquation :: (Var, Var) -> Equation -> Equation
applySubstitutionToEquation substitution (side1, side2) = 
  (applySubstitutionToSide substitution side1, applySubstitutionToSide substitution side2)

applySubstitutionToGamma :: (Var, Var) -> Problem -> Problem
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

expandChainVariableReverse :: Bind -> Int -> [Bind]
expandChainVariableReverse (CV _ startVar endVar) 1 = [B startVar endVar]
expandChainVariableReverse (CV name startVar endVar) expandSize = 
  (B newEndVar endVar)
  :(expandChainVariableReverse (CV name startVar newEndVar) (expandSize - 1))
  where
    newEndVar = Meta ("CV" ++ name ++ (show (expandSize - 1)))


expandChainVariable :: Bind -> Int -> [Bind]
expandChainVariable cv expandSize = reverse (expandChainVariableReverse cv expandSize)

















