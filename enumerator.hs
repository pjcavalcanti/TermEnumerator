import qualified Data.Set as Set

data Term = TermVar Int | TermApp Term Term | TermLambda Int Term
  deriving (Eq, Ord)

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

allTermsNVars :: Int -> [Term]
allTermsNVars nVars = concat $ iterate (\l -> growList l nVars) [(TermVar x) | x <- [1..nVars]]

allTerms = [allTermsNVars i !! (n - i) | n <- [1..], i <- [1..n]]

see_n :: (Ord a, Show a) => Int -> [a] -> IO ()
see_n n xs = aux n Set.empty xs
  where
    aux 0 _ _      = return ()
    aux _ _ []     = return ()
    aux m seen (x:xs)
      | x `Set.member` seen = aux m seen xs
      | otherwise = do
          print x
          aux (m-1) (Set.insert x seen) xs

-- For the sake of readability
prettyTerm :: Term -> String
prettyTerm (TermVar x) = show x
prettyTerm (TermApp t1 t2) = "(" ++ (prettyTerm t1)++ " " ++ (prettyTerm t2) ++ ")"
prettyTerm (TermLambda x t) = "(λ" ++ (show x)++ " . " ++ (prettyTerm t) ++ ")"

instance Show Term where
  show = prettyTerm
