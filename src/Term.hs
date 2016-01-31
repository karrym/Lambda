
module Term where

import Parser
import qualified Data.IntMap as IM
import Data.IORef (IORef)

type Env = IM.IntMap (IORef Memory)

data Memory = Thunk Env IxLam
            | Value IxLam

data Lambda = Var String 
            | App Lambda Lambda
            | Lam String Lambda
            | Let String Lambda Lambda
            | Fix String Lambda
            deriving (Eq)

data IxLam = Index Int
           | IxApp IxLam IxLam
           | IxLam IxLam
           | IxLet IxLam IxLam
           | Clojure Env IxLam
           | IxFix IxLam
           deriving (Eq)

instance Show Lambda where
        show = showExpr 0 where
            open  a b = if a > b then "(" else ""
            close a b = if a > b then ")" else ""
            showExpr :: Int -> Lambda -> String
            showExpr _ (Var s)   = s
            showExpr n (Let x a b) = open n 0 ++ "let " ++ x ++ " = " ++ showExpr 0 a ++ " in " ++ showExpr 0 b ++ close n 0
            showExpr n (Lam x l) = open n 1 ++ 'λ':x ++ '.':showExpr 1 l ++ close n 1
            showExpr n (App a b) = open n 2 ++ showExpr 2 a ++ " " ++ showExpr 3 b ++ close n 2
            showExpr n (Fix f e) = open n 4 ++ "fix " ++ f ++ ' ' : showExpr 4 e ++ close n 4

instance Show IxLam where
        show = showExpr 0 where
            open  a b = if a > b then "(" else ""
            close a b = if a > b then ")" else ""
            showExpr :: Int -> IxLam -> String
            showExpr _ (Index i)   = show i
            showExpr _ (Clojure v a) = "(Clojure)[λ." ++ showExpr 0 a ++ "]"
            showExpr n (IxLet a b) = open n 0 ++ "let . = " ++ showExpr 0 a ++ " in " ++ showExpr 0 b ++ close n 0
            showExpr n (IxLam l)   = open n 1 ++ "λ." ++ showExpr 1 l ++ close n 1
            showExpr n (IxFix e) = open n 2 ++ "fix " ++ showExpr 2 e ++ close n 2
            showExpr n (IxApp a b) = open n 3 ++ showExpr 3 a ++ " " ++ showExpr 4 b ++ close n 3

instance Show Memory where
        show (Thunk _ l) = "Thunk:" ++ show l
        show (Value l) = "Value:" ++ show l

assign :: Parser Lambda
assign = Let <$> (spaces *> string "let" *> token) <*> (string ":=" *> lambda) <*> (spaces *> string "in" *> assign)
      <|> lambda

lambda :: Parser Lambda
lambda = flip (foldr Lam) <$> (spaces *> (char '\\' <|> char 'λ') *> some token <* char '.') <*> lambda
      <|> apply

apply :: Parser Lambda
apply = chainl (spaces *> fixed) (pure App)

fixed :: Parser Lambda
fixed = Fix <$> (string "fix" *> token) <*> factor
     <|> factor

factor :: Parser Lambda
factor = Var <$> token
      <|> char '(' *> assign <* char ')'
