module Types where

data Bind = B Var Var | CV String Var Var deriving(Show, Eq)
data Var = Concrete String | Meta String deriving(Show, Eq)
type Problem = [Equation] 
type Equation = (Side, Side)
data Side = BL [Bind] | V Var deriving(Show, Eq)
type Sol = [SolEntry] 
data SolEntry = Sub Var Var | Exp String Int deriving(Show, Eq)

