module Type
  ( VarIndex, Term(..), Rule(..), Prog(..), Goal(..)
  ) where

-- Alias type for variables
type VarIndex = Int

-- Data type for terms
data Term = Var VarIndex | Comb String [Term]

-- Data type for program rules
data Rule = Term :- [Term]

-- Data type for programs
data Prog = Prog [Rule]

-- Data type for goals
data Goal = Goal [Term]

class Pretty a 
    where pretty :: a -> String
    
instance Pretty Term 
    where
        pretty (Var v)         = (prettyVarIndex v)
        pretty (Comb s (x:xs)) = s ++ "(" ++ (separate "," (x:xs)) ++ ")"
        
instance Pretty Rule
    where
        pretty (t :- (x:xs))   = pretty t ++ " :- " ++ (separate "," (x:xs)) ++ "."
        
instance Pretty Prog
    where
        pretty (Prog (x:xs))   = "Prog:" ++ "\n" ++ (separate "\n" (x:xs))
        
instance Pretty Goal
    where
        pretty (Goal (x:xs))   = "Goal: " ++ (separate "," (x:xs))

prettyVarIndex :: VarIndex -> String
prettyVarIndex x = varNames!!x
    where
        varNames = [if j == "0" then i else i ++ j | j <- map show [0..], i <- map (:[]) ['A'..'Z']]
      
separate _ []     = ""
separate _ [x]    = pretty x
separate s (x:xs) = pretty x ++ s ++ (separate s xs)

-- Data type for substitutions (correct?)
data Subst = Subst [(Term,Term)]

       