{-# LANGUAGE OverloadedStrings #-}

module Template.Math.RPN
  (evalRPN, EnvVars)
where

import Template.Prelude hiding (show)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import qualified Data.HashMap.Strict as Map

-- import GHC.Show (Show(..))

type EnvVars = Map.HashMap Text Integer
type Stack = [Integer]
type EvalM = ReaderT EnvVars (ExceptT EvalError (State Stack))

data EvalError = NotEnoughElements | NotANumber Text | ExtraElements

printNotANumber err = "Can't convert " <> err <> " into an integer"
printExtraElements = "The expression could not be fully resolved"
printNotEnoughElements = "Not enough elements in the expression"

instance Print EvalError where
  hPutStr h (NotANumber err) = liftIO $ Text.hPutStr h (printNotANumber err)
  hPutStr h ExtraElements = liftIO $ Text.hPutStr h printExtraElements
  hPutStr h NotEnoughElements = liftIO $ Text.hPutStr h printNotEnoughElements
  hPutStrLn h (NotANumber err) = liftIO $ Text.hPutStrLn h (printNotANumber err)
  hPutStrLn h ExtraElements = liftIO $ Text.hPutStrLn h printExtraElements
  hPutStrLn h NotEnoughElements = liftIO $ Text.hPutStrLn h printNotEnoughElements

push :: Integer -> EvalM ()
push x = modify (x:)

pop :: EvalM Integer
pop = do
  xs <- get
  when (null xs) $
    throwError NotEnoughElements
  put (tailSafe xs)
  case headMay xs of
    Just s -> pure s
    Nothing -> throwError NotEnoughElements -- "The head of the stack is empty"

checkResult :: EvalM ()
checkResult = do
  xs <- get
  when (length xs /= 1) $
    throwError ExtraElements

handleNaN :: Text -> Maybe Integer -> EvalM Integer
handleNaN s Nothing = throwError (NotANumber s)
handleNaN _ (Just n ) = pure n

readSafe :: Text -> EvalM Integer
readSafe t = do
  readSafe' t `catchError` handler
  where
    readSafe' t = handleNaN t $ hush $ second fst (Text.decimal t)
    handler (NotANumber n) = asks (Map.lookup n) >>= handleNaN t
    handler e = throwError e

evalRPN :: Text -> EnvVars -> Either EvalError Integer
evalRPN expr env = evalState (runExceptT (runReaderT evalRPN' env)) []
  where
    evalRPN' = traverse_ step (Text.words expr) >> checkResult >> pop
    step "+" = process_tops (+)
    step "-" = process_tops (-)
    step "*" = process_tops (*)
    step t   = readSafe t >>= push
    process_tops op = flip op <$> pop <*> pop >>= push
