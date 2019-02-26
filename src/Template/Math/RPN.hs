{-# LANGUAGE OverloadedStrings #-}

module Template.Math.RPN
  ( evalRPN)
where

import Template.Prelude
import qualified Data.Text as Text
import qualified Data.Text.Read as Text

type Stack = [Integer]
type ErrMsg = Text
type EvalM = ExceptT ErrMsg (State Stack)

push :: Integer -> EvalM ()
push x = modify (x:)

pop :: EvalM Integer
pop = do
  xs <- get
  when (null xs) $
    throwError "Trying to pop from an empty stack"
  put (tailSafe xs)
  case headMay xs of
    Just s -> pure s
    Nothing -> throwError "The head of the stack is empty"

checkResult :: EvalM ()
checkResult = do
  xs <- get
  when (length xs /= 1) $
    throwError "The expression could not be fully resolved"

readSafe :: Text -> EvalM Integer
readSafe t = do
  let err = "Can't convert " <> t <> " into an integer"
  ExceptT $ pure $ bimap (const err) fst (Text.decimal t)
  -- case Text.decimal t of
  --   Right (s,_) -> pure s
  --   Left _ -> throwError err

evalRPN :: Text -> Either ErrMsg Integer
evalRPN expr = evalState (runExceptT evalRPN') []
  where
    evalRPN' = traverse_ step (Text.words expr) >> checkResult >> pop
    step "+" = process_tops (+)
    step "-" = process_tops (-)
    step "*" = process_tops (*)
    step t   = readSafe t >>= push
    process_tops op = flip op <$> pop <*> pop >>= push
