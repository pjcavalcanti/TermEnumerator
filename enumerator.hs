import Data.Tree (Tree(..))
import Data.Tree.Pretty (drawVerticalTree)

data Type = TypeVar Int | TypeArrow Type Type
  deriving (Eq, Show)
data Term = TermVar Int | TermApp Term Term | TermLambda Int Type Term
  deriving (Eq)

termToTree :: Term -> Tree String
termToTree (TermVar v) = Node ("" ++ show v) []
termToTree (TermApp t1 t2) = Node "Appl" [termToTree t1, termToTree t2]
termToTree (TermLambda v ty t) =
  Node ("Lambda" ++ show v ++ " :: " ++ typeToStr ty)
       [termToTree t]

typeToStr :: Type -> String
typeToStr (TypeVar x)         = "TypeVar " ++ show x
typeToStr (TypeArrow ty1 ty2) = "(" ++ typeToStr ty1 ++ " -> " ++ typeToStr ty2 ++ ")"

prettyTerm :: Term -> String
prettyTerm = drawVerticalTree . termToTree

instance Show Term where
  show = prettyTerm

grow :: Term -> Int -> [Term]
grow (TermVar v) nVars = [(TermApp (TermVar x) (TermVar y)) | x <- [1..nVars], y <- [1..nVars]]
grow (TermApp t1 t2) nVars = [(TermApp nt1 nt2) | nt1 <- (grow t1 nVars), nt2 <- (grow t2 nVars)]
grow (TermLambda v type t) nVars = [(TermLambda v type nt) | nt <- (grow t nVars)]
