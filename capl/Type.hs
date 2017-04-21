module Type
--  ( VarIndex, Term(..), Rule(..), Prog(..), Goal(..)) 
 where
import Data.List

-- Alias type for variables
type VarIndex = Int

varIndexList = (map (\x -> [x]) ['A'..'Z']) ++ [x:(show y) | y <- [1..], x <- ['A'.. 'Z']]

-- Data type for terms
data Term = Var VarIndex | Comb String [Term]
    deriving (Eq, Show)

-- Data type for program rules
data Rule = Term :- [Term]


-- Data type for programs
data Prog = Prog [Rule]

-- Data type for goals
data Goal = Goal [Term]
	deriving Show

-- Data type for substitutions
data Subst = Subst [(VarIndex, Term)]
          deriving (Show, Eq)

-- Data type for SLD Trees
data SLDTree = Tree Goal [(Subst, SLDTree)]
	deriving Show


-- class for pretty output
class Pretty a where
  pretty :: a -> String

-- pretty instance for data type term
instance Pretty Term where
   pretty (Var x) =  (varIndexList !! x)
   pretty (Comb string []) = string
   pretty (Comb string xs) = string ++ "(" ++ concat(intersperse "," (foldr (\x y -> (pretty x) : y) [] xs)) ++ ")"


--  pretty instance for data type rule
instance Pretty Rule where
   pretty (x :- []) = (pretty x) ++ "."
   pretty (x :- xs) = (pretty x) ++ " :- " ++ init (foldr (\x y -> ((pretty x) ++ ",") ++ y) "" xs) ++ "."


-- pretty instance for data type prog
instance Pretty Prog where
   pretty (Prog xs) = (foldr (\x y -> ((pretty x) ++ "\n") ++ y) "" xs)


-- pretty instance for data type goal
instance Pretty Goal where
   pretty (Goal [])  = "?- ."
   pretty (Goal xs) = "?- " ++ init (foldr (\x y -> ((pretty x) ++ ",") ++ y) "" xs) ++ "."


--  pretty instance for data type subst
instance Pretty Subst where
   pretty(Subst []) = "{}"
   pretty (Subst xs) = "{" ++ init (foldr (\x y -> (pretty (Var (fst x)) ++ " --> " ++ (pretty (snd x)))++ "," ++ y) "" xs) ++ "}"


-- pretty instance for SLD trees
instance Pretty SLDTree where
	pretty x = prettyTree 0 x
	   where prettyTree n (Tree y []) = (showGoal n y)
	         prettyTree n (Tree y xs) = intercalate "\n" ((showGoal n y): concat(map(\(a,b)->((showSub (n+1) a):[(prettyTree(n+1) b)]))xs))
	         showSub n x = concat (replicate (n-1) "|    ") ++"+-- " ++ pretty x
	         showGoal 0 x = pretty x
	         showGoal n x = concat (replicate n "|    ")++pretty x



--composes two substitutions
compose :: Subst -> Subst -> Subst
compose (Subst xs) (Subst ys) = Subst (compose' xs ys)

compose' :: [(VarIndex, Term)] -> [(VarIndex, Term)] -> [(VarIndex, Term)]
compose' xs [] = xs
compose' [] ys = ys
compose' xs (y:ys) 
            | elem (snd y) (dom xs) = (compHelp y xs) : (compose' xs ys)
            | otherwise             = y : (compose' xs ys)
                 where compHelp :: (VarIndex, Term) -> [(VarIndex, Term)] -> (VarIndex, Term)
                       compHelp y (z:zs)  | (snd y) == (Var(fst z)) = (fst y, snd z)
                                          | otherwise               = compHelp y zs

-- creates the dom of a substitution
dom :: [(VarIndex, Term)] -> [Term]
dom xs = map (\x -> (Var x)) (fst (unzip xs))


-- applies a substitution to a term
apply :: Subst -> Term -> Term
apply (Subst [])  t           = t
apply xs         (Comb s ts)  = (Comb s (map (apply xs) ts))
apply (Subst (x:xs)) t  
           |(Var(fst x)) == t = (snd x)
           |otherwise         = apply (Subst xs) t




-- creates the disagreementset of two terms
ds :: Term -> Term -> Maybe (Term, Term)
ds x y 
   | x /=y     = dsHelp x y
   | otherwise = Nothing
   where dsHelp (Var z) q = Just ((Var z), q)
         dsHelp z (Var q) = Just (z, (Var q))
         dsHelp t1@(Comb z zs) t2@(Comb q qs) 
             | z /= q ||(length zs) /= (length qs)   = Just (t1,t2)
             | otherwise                             = dsHelp2 zs qs
             where dsHelp2 (a:as) (u:us) | a == u    = dsHelp2 as us
                                       | otherwise   = Just (a, u)

-- unification of terms
unify::Term -> Term -> Maybe Subst
unify t1 t2 = uni t1 t2 (Subst []) -- empty set for the iteration
     where uni t1 t2 c = let c1 = ds (apply c t1) (apply c t2)
                         in  helper c1 t1 t2 c 
                            where helper (Just ((Var x), t)) t1 t2 c | testForVariable x t = Nothing
                                                                     | otherwise           = uni t1 t2 (compose (Subst[(x,t)]) c)
                                  helper (Just (t ,(Var x))) t1 t2 c | testForVariable x t = Nothing
                                                                     | otherwise           = uni t1 t2 (compose (Subst[(x,t)]) c)
                                  helper (Just (t ,v )) t1 t2 c = Nothing
                                  helper Nothing t1 t2 c = Just c



-- tests if a variable is part of a term.
testForVariable :: VarIndex -> Term -> Bool
testForVariable x (Var y)     = x == y
testForVariable x (Comb _ ys) = foldr (\s y -> (testForVariable x s) || y) False ys 





-- generates the SLDTree for a given program and a given goal.
sld:: Prog -> Goal -> SLDTree
sld   p g@(Goal xs)   = sld' p g (maximum(makeListofVariables g)+1)
sld'  _ (Goal [])   n = Tree (Goal []) []
sld'  p g@(Goal xs) n = let rules = reName (n + 2) (searchRules (head xs) p) 
			 in makeTree rules p g (n+2)
			        where makeTree rules p o@(Goal ((Comb "not" xs):ys)) m = case sld' p (Goal xs) m of
											 (Tree (Goal xs) []) -> Tree (Goal ys) []
											 (Tree _ _)          -> Tree  o []
			              makeTree rules p g    m                         = Tree g (concat (map (\f -> fabulousFunction f g) rules))
					    where fabulousFunction rule (Goal goals) = let subs = putSubstTogether [(unify (head goals) (getLeftSide rule))] 
									                   in tryToApply subs rule 
											      where tryToApply Nothing      rule = []
											            tryToApply (Just subst) rule = [(subst, sld' p (Goal((map (\a -> apply subst a) (getRightSide rule))
																		++ (map(\a -> apply subst a) (tail xs)))) m)]
-- renames the variables in a list of rules
reName :: Int -> [Rule] -> [Rule]
reName _ []           = []
reName n (rule:rules) = (reName' n rule) :(reName n rules) 
	where reName' :: Int -> Rule -> Rule
	      reName' n (a@(Comb s xs) :- ts) = let stupidList =   nub ((makeListofVariables (Goal (xs )))++makeListofVariables (Goal (ts)))
	                                            subs       = Subst (makeRNSub  (max n (maximum stupidList)+1) stupidList)
						                              in  (apply subs a) :- (map (\x -> apply subs x) ts)
				
-- makes the substitution needed to rename a Term
makeRNSub :: Int -> [VarIndex] -> [(VarIndex, Term)]
makeRNSub _ []     =  []
makeRNSub n all@(x:xs) =if x<n then (x, (Var n)) : (makeRNSub (n+1) xs) else makeRNSub (n+1) all


-- not so pretty helpfunktion
putSubstTogether :: [Maybe Subst] -> Maybe Subst
putSubstTogether xs 
        | allNothing xs = Nothing
	| otherwise     = putSubstTogether' xs
		where putSubstTogether' []	                   = Just (Subst [])
		      putSubstTogether' (Nothing : xs)         = putSubstTogether' xs
		      putSubstTogether' ((Just (Subst x)) : xs)= let Just (Subst y) = putSubstTogether' xs 
                                          				 in Just (Subst (x ++ y))


--allNothing :: [Maybe a] -> Bool
allNothing []           = True
allNothing (x:xs)
         | x == Nothing = allNothing xs
		     | otherwise    = False




--- gives a list of all rules that occur in the program an match the given term
searchRules :: Term -> Prog -> [Rule]
searchRules (Var _) _             = error "Das Programm ist blöd"
searchRules (Comb s xs) (Prog []) = []
searchRules (Comb s xs) (Prog (ac@((Comb a bs):- cs):ys)) 
                      | a == s    = ac : searchRules (Comb s xs) (Prog ys)
                      | otherwise = searchRules (Comb s xs) (Prog ys)



-- getter methods for rules
getRightSide:: Rule -> [Term]
getRightSide (_:- rt)= rt

getLeftSide:: Rule -> Term
getLeftSide (rl:-_) = rl



--depth-first search
dfs:: SLDTree -> [Subst]
dfs (Tree _ [])  = []
dfs t@(Tree g xs) = help t (Subst[]) 100
            where help _                subs 0  = []
		  help (Tree (Goal []) _) subs _ = [subs]
                  help (Tree g []) subs _         = []
                  help (Tree g ys) subs n         = foldr (\(a,b) s -> (help b (compose a subs) (n-1)) ++ s) [] ys
                        

--Breadth First Search
bfs:: SLDTree -> [Subst]
bfs (Tree _ []) = []
bfs t = help t [] (Subst []) 100
               where help _                subs sub 0  = []
		     help (Tree (Goal []) _) subs sub _ = subs
                     help (Tree g []) subs sub _         = []
                     help (Tree g ys) subs sub n = let subsis = foldr (\(a,b) s -> if (isEmptyTree b) then (compose a sub) : s else s) [] ys -- list of succesfull substs
					               nodes  = foldr (\(a,b) s -> if not(isEmptyTree b) then ((compose a sub),b) : s else s) [] ys -- list of nodes we just visited
						    in foldr (\(a,b) s -> (help b [] a (n-1)) ++ s) subsis nodes 

isEmptyTree :: SLDTree -> Bool
isEmptyTree (Tree (Goal []) _) = True
isEmptyTree _ = False

tree_list:: SLDTree ->[(Subst, SLDTree)]
tree_list (Tree g xs) = xs

-- uses a given strategy to solve the goal from a given program
solve:: (SLDTree -> [Subst]) -> Prog -> Goal -> [Subst]
solve f p g = f (sld p g) --func composition


-- makes a list of all variables in a goal
makeListofVariables :: Goal -> [VarIndex]
makeListofVariables (Goal []) = []
makeListofVariables (Goal (x:xs))= case x of
				   (Var y) -> [y]
				   (Comb str ys) -> (makeListHelp ys) ++ (makeListofVariables (Goal xs))
							where makeListHelp [] = []
							      makeListHelp ((Var z):zs) = [z] ++ (makeListHelp zs)
							      makeListHelp ((Comb str os):zs)= (makeListHelp os) ++ (makeListHelp zs)


-- generates a pretty stringoutput for the results generated by solve
match :: [VarIndex] -> [Subst] -> [String]
match [] [(Subst [])] 	= ["true"]
match [] _      = []
match _ []      = ["false"]
match (x:xs) ys = foldr (\g f -> ((matchHelp x g) : (match xs ys)) ++ f) [] ys
   where matchHelp x (Subst ((a,b):abs))  | x == a = ((pretty (Var x)) ++ "=" ++ (pretty b))
					  | otherwise = matchHelp x (Subst abs)
	 matchHelp x (Subst []) = "something went very wrong"











