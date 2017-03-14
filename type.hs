module Type
  ( VarIndex, Term(..), Rule(..), Prog(..), Goal(..)
  ) where

-- Alias type for variables
type VarIndex = Int

-- Data type for terms
data Term = Var VarIndex | Comb String [Term]
    deriving Show

-- Data type for program rules
data Rule = Term :- [Term]

-- Data type for programs
data Prog = Prog [Rule]

-- Data type for goals
data Goal = Goal [Term]

--------------------------------------------- Aufgabe 1 ------------------------------------------------

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

--------------------------------------------- Aufgabe 2 ------------------------------------------------

-- Data type for substitutions
data Subst = Subst [(Term,Term)]

instance Pretty Subst
    where 
        pretty (Subst (x:xs)) = "o = {" ++ separateTerm (Subst (x:xs)) ++ "}"
        
separateTerm (Subst [])  = ""
separateTerm (Subst [x]) = prettyTerm x
separateTerm (Subst (x:xs)) = prettyTerm x ++ ", " ++ (separateTerm (Subst xs))

prettyTerm (x,y) = pretty x ++ " -> " ++ pretty y

compose :: Subst -> Subst -> Subst
compose s1 s2 = remove_tautologies (add_missing_terms (compose'' s1 s2) s2)

compose'' :: Subst -> Subst -> Subst
compose'' x (Subst [])                  = x
compose'' (Subst (x:xs)) (Subst (y:ys)) = compose'' (Subst (compose' y (x:xs))) (Subst ys)

compose' :: (Term,Term) -> [(Term,Term)] -> [(Term,Term)]
compose' _ []                     = []
compose' (Var y1,t2) ((t3,t4):xs) = ((t3,(replace (Var y1) t2 t4)):(compose' (Var y1,t2) xs))

replace :: Term -> Term -> Term -> Term
replace (Var y) t (Var x)         = if x == y then t else (Var x)
replace (Var y) t (Comb s (x:xs)) = (Comb s (map (replace (Var y) t)(x:xs)))

remove_tautologies :: Subst -> Subst
remove_tautologies (Subst ts) = (Subst (remove_tautologies_list ts))

remove_tautologies_list :: [(Term,Term)] -> [(Term,Term)]
remove_tautologies_list [] = []
remove_tautologies_list ((t1,t2):ts) = if t1 == t2 then remove_tautologies_list ts else ((t1,t2):remove_tautologies_list ts)

instance Eq Term
    where 
        (Var x)      == (Var y)       = x  == y
        (Comb s1 ts) == (Comb s2 tss) = s1 == s2 && ts == tss
        _            == _             = False
        t1           /= t2            = not (t1 == t2)
        
add_missing_terms :: Subst -> Subst -> Subst
add_missing_terms (Subst ts) (Subst tss) = (Subst (add_missing_terms_list ts tss))

add_missing_terms_list :: [(Term,Term)] -> [(Term,Term)] -> [(Term,Term)]
add_missing_terms_list ts      []       = ts
add_missing_terms_list (t1:ts) (t2:tss) = if isElem t2 (t1:ts) then add_missing_terms_list (t1:ts) tss else add_missing_terms_list (t2:(t1:ts)) tss

isElem :: (Term,Term) -> [(Term,Term)] -> Bool
isElem _ [] = False
isElem (t1,t2) ((t3,t4):ts) = if t1 == t3 then True else isElem (t1,t2) ts
        
apply :: Subst -> Term -> Term
apply (Subst ts) t = apply_list ts t

apply_list :: [(Term,Term)] -> Term -> Term
apply_list []           t = t
apply_list ((t1,t2):ts) t = apply_list ts (replace t1 t2 t)

--------------------------------------------- Aufgabe 3 ------------------------------------------------

-- Disagreement Set (Unstimmigkeitsmenge)
ds :: Term -> Term -> Maybe (Term,Term)
ds t1           t2            = if t1 == t2      then nothing           else ds t1 t2 -- ? --
ds (Var v)      t2            = if (Var v) /= t2 then Just ((Var v),t2) else nothing
ds t1           (Var v)       = if t1 /= (Var v) then Just (t1,(Var v)) else nothing
ds (Comb s1 ts) (Comb s2 tss) = if s1 /= s2 || (length ts) /= (length tss) then Just ((Comb s1 ts),(Comb s2 tss)) else if s1 == s2 && (length ts) == (length tss) then 


-- Unifikation
unify :: Term -> Term -> Maybe Subst
unify = undefined























