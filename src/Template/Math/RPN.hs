{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wunused-top-binds¶ #-}

module Template.Math.RPN ()
where

import Template.Prelude
import qualified Data.Text as Text
import qualified Data.Text.Read as Text

type Stack = [Integer]
type ErrMsg = Text
type EvalM = StateT Stack (Either ErrMsg)

push :: Integer -> EvalM ()
push x = modify (x:)

err :: ErrMsg -> EvalM ()
err = lift . Left

pop :: EvalM Integer
pop = do
  xs <- get
  when (null xs) $
    err "Invalid input: trying to pop from an empty stack"
  put (tailSafe xs)
  lift $ maybeToRight "Invalid input: the head of the stack is empty" $  headMay xs

checkResult :: EvalM ()
checkResult = do
  xs <- get
  when (length xs /= 1) $
    err "Invalid input: the expression could not be fully resolved"

evalRPN :: Text -> Either ErrMsg Integer
evalRPN expr = evalStateT evalRPN' []
  where
    evalRPN' = traverse step (Text.words expr) >> checkResult >> pop
    step "+" = process_tops (+)
    step "-" = process_tops (-)
    step "*" = process_tops (*)
    step t = do
      let err_msg = "Can't convert " <> t <> " into an integer"
      (s, _) <- lift $ first (const err_msg) (Text.decimal t)
      push s
    process_tops op = flip op <$> pop <*> pop >>= push
