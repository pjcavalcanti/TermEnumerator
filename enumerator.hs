data Term = TermVar Int | TermApp Term Term | TermLambda Int Term
  deriving (Eq)

prettyTerm :: Term -> String
prettyTerm (TermVar x) = show x
prettyTerm (TermApp t1 t2) = "(" ++ (prettyTerm t1)++ " " ++ (prettyTerm t2) ++ ")"
prettyTerm (TermLambda x t) = "(λ" ++ (show x)++ " . " ++ (prettyTerm t) ++ ")"

instance Show Term where
  show = prettyTerm

growTerm :: Term -> Int -> [Term]
growTerm (TermVar v) nVars =
  [(TermApp (TermVar x) (TermVar y)) | x <- [1..nVars], y <- [1..nVars]]
  ++ [(TermLambda x (TermVar y)) | x <- [1..nVars], y <- [1..nVars]]
growTerm (TermApp t1 t2) nVars =
  [(TermApp nt1 nt2) | nt1 <- (growTerm t1 nVars), nt2 <- (growTerm t2 nVars)]
growTerm (TermLambda v t) nVars =
  [(TermLambda v nt) | nt <- (growTerm t nVars)]

growList :: [Term] -> Int -> [Term]
growList ts nVars = foldr (\t r -> ((\t' -> growTerm t' nVars) t ++) . r) id ts []

getTerms :: Int -> Int -> [Term]
getTerms 0 nVars = [(TermVar x) | x <- [1..nVars]]
getTerms n nVars = growList (getTerms (n-1) nVars) nVars
