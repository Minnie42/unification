{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Types where
import Data.List

data Bind = B Var Var | CV String Var Var deriving(Eq)
instance Show Bind where
 show (B v1 v2)    = concat [show v1, "=", show v2] 
 show (CV x v1 v2) = concat ["CV",x,"[",show v1,",",show v2,"]"] 
instance Ord Bind where
  compare (B _ _) (CV _ _ _) = LT
  compare (CV _ _ _) (B _ _) = GT
  compare bind1 bind2 = countMeta bind1 `compare` countMeta bind2
    where 
      isMeta (Meta _) = True
      isMeta _ = False 
      countMeta (B var1 var2) = (if isMeta var1 then 1 else 0) + (if isMeta var2 then 1 else 0)
      countMeta (CV _ var1 var2) = (if isMeta var1 then 1 else 0) + (if isMeta var2 then 1 else 0)

data Var = Concrete String | Meta String deriving(Eq)
instance Show Var where
 show (Concrete v) = v
 show (Meta v) = v

type Problem = [Equation] 
type Equation = (Side, Side)

data Side = BL [Bind] | V Var deriving(Eq)
instance Show Side where
  show (V v)  = show v
  show (BL xs) = intercalate ";" (map show xs) 

type Sol = [SolEntry] 
data SolEntry = Sub Var Var | Exp String Int deriving(Eq)
instance Show SolEntry where
  show (Sub v1 v2) = concat [show v1,"|->",show v2]
  show (Exp s i) = (concat ["CV",s,"|->", "[.]="])
                   ++ (case (intercalate "=" ["CV" ++ s ++ (show j) ++ "; CV" ++ s ++ (show (j)) | j <- [1..i-1]]) of 
                         [] -> []
                         xs -> xs ++ "=")
                   ++ ("[.]")
