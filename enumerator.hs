import Data.Tree (Tree(..))
import Data.Tree.Pretty (drawVerticalTree)

data Term = TermVar Int | TermApp Term Term | TermLambda Int Term
  deriving (Eq)

termToTree :: Term -> Tree String
termToTree (TermVar v) = Node ("" ++ show v) []
termToTree (TermApp t1 t2) = Node "Appl" [termToTree t1, termToTree t2]
termToTree (TermLambda v t) = Node ("λ" ++ show v) [termToTree t]

prettyTerm :: Term -> String
prettyTerm = drawVerticalTree . termToTree

instance Show Term where
  show = prettyTerm

  -- Wrapper type for list of Terms
newtype TermList = TermList [Term]
unwrap :: TermList -> [Term]
unwrap (TermList l) = l

instance Show TermList where
  show (TermList terms) = unlines (map show terms)

growTerm :: Term -> Int -> TermList
growTerm (TermVar v) nVars = TermList (
  [(TermApp (TermVar x) (TermVar y)) | x <- [1..nVars], y <- [1..nVars]]
  ++ [(TermLambda x (TermVar y)) | x <- [1..nVars], y <- [1..nVars]])
growTerm (TermApp t1 t2) nVars =
  TermList [(TermApp nt1 nt2) | nt1 <- unwrap (growTerm t1 nVars), nt2 <- unwrap (growTerm t2 nVars)]
growTerm (TermLambda v t) nVars =
  TermList [(TermLambda v nt) | nt <- unwrap (growTerm t nVars)]

growList :: TermList -> Int -> TermList
growList ts nVars = TermList $ foldr (\t r -> ((unwrap .(\t' -> growTerm t' nVars)) t ++) . r) id (unwrap ts) []

getTerms :: Int -> Int -> TermList
getTerms 0 nVars = TermList [(TermVar x) | x <- [1..nVars]]
getTerms n nVars = growList (getTerms (n-1) nVars) nVars
