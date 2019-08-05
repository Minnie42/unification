 module Types where

data Bind = B Var Var deriving(Show, Eq)
data Var = Concrete String | Meta String deriving(Show, Eq)
type Problem = [Equation] 
type Equation = (Side, Side)
data Side = BL [Bind] | V Var deriving(Show, Eq)
type Sol = [(Var, Var)]
