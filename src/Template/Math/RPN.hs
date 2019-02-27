{-# LANGUAGE OverloadedStrings #-}

module Template.Math.RPN
  (evalRPN, EnvVars, readDecimal)
where

import qualified Data.HashMap.Strict       as Map
import qualified Data.Text                 as Text
import           Data.Text.Prettyprint.Doc
import qualified Data.Text.Read            as Text
import           Template.Prelude          hiding (show)

data EvalError = NotEnoughElements | NotANumber Text | ExtraElements

instance Pretty EvalError where
  pretty (NotANumber err) = pretty ("Can't convert " <> err <> " into an integer") <> line
  pretty ExtraElements = pretty ("The expression could not be fully resolved" :: Text) <> line
  pretty NotEnoughElements = pretty ("Not enough elements in the expression" :: Text) <> line

type EnvVars = Map.HashMap Text Integer
type Stack = [Integer]
type EvalM = ReaderT EnvVars (ExceptT EvalError (State Stack))

push :: Integer -> EvalM ()
push x = modify (x:)

pop :: EvalM Integer
pop = do
  xs <- get
  when (null xs) $
    throwError NotEnoughElements
  put (tailSafe xs)
  case headMay xs of
    Just s  -> pure s
    Nothing -> throwError NotEnoughElements -- "The head of the stack is empty"

checkResult :: EvalM ()
checkResult = do
  xs <- get
  when (length xs /= 1) $
    throwError ExtraElements

handleNaN :: Text -> Maybe Integer -> EvalM Integer
handleNaN s Nothing   = throwError (NotANumber s)
handleNaN _ (Just n ) = pure n


readDecimal :: Text -> Maybe Integer
readDecimal = hush . second fst . Text.decimal

readSafe :: Text -> EvalM Integer
readSafe t = do
  readSafe' t `catchError` handler
  where
    readSafe' t = handleNaN t (readDecimal t)
    handler (NotANumber n) = asks (Map.lookup n) >>= handleNaN t
    handler e              = throwError e

evalRPN :: Text -> EnvVars -> Either EvalError Integer
evalRPN expr env = evalState (runExceptT (runReaderT evalRPN' env)) []
  where
    evalRPN' = traverse_ step (Text.words expr) >> checkResult >> pop
    step "+" = process_tops (+)
    step "-" = process_tops (-)
    step "*" = process_tops (*)
    step t   = readSafe t >>= push
    process_tops op = flip op <$> pop <*> pop >>= push
