module Template.Math.Expr where

import Template.Prelude
import Data.Maybe (fromJust)

data Expr a
  = Lit a
  | Add (Expr a) (Expr a)
  | Mult (Expr a) (Expr a)

type Token = [Char]
type Stack = [Token]
type Output = [Expr Integer]
type AppState = (Stack, Output)


output :: Token -> State AppState ()
output = modify . second . builder
  where
    builder "+" (e1 : e2 : es) =  Add e1 e2 : es
    builder "*" (e1 : e2 : es) =  Mult e1 e2 : es
    builder n es = Lit (fromJust $ readMaybe n) : es -- TODO deal with Nothing read

tokenize = groupBy (\ a b -> isDigit a && isDigit b)
           . filter (not . isSpace)

main = panic "todo"
