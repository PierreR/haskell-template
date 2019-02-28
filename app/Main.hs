{-# LANGUAGE OverloadedLists #-}
module Main where

import qualified Data.HashMap.Strict                   as Map
import           Data.Text.Prettyprint.Doc             (pretty)
import           Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import           Template.Math.RPN                     (EnvVars)
import qualified Template.Math.RPN                     as RPN

import           Template.Prelude

printResult :: Text -> EnvVars -> IO ()
printResult expr env = either (putDoc . pretty) print (RPN.evalRPN expr env)

askVar :: IO (Text, Integer)
askVar = do
  putText "Variable name ?"
  var <- getLine
  putText "Value ?"
  val0 <- getLine
  case RPN.readDecimal val0 of
    Nothing -> do
      putText $  "Sorry cannot convert the value " <> val0 <> " into an integer"
      putText "Let's try again"
      askVar
    Just v -> pure (var, v)

-- A Prompt is an interactive loop that will prompt the user for values
-- in order to build 'EnvVars'.
-- The loop is broken by the user and should always return.
type Prompt = MaybeT (StateT EnvVars IO) EnvVars
envPrompt :: Prompt
envPrompt = do
  (var, val)<- liftIO askVar
  modify (Map.insert var val)
  putText "Do you want to setup other variables ? If yes press y"
  input_from_user <- liftIO getLine
  if input_from_user == "y"
    then continue
    else break =<< get

getEnvVar :: IO EnvVars
getEnvVar = do
  putText "Need to set values ? (y or enter to ignore)"
  i <- getLine
  if i == "y"
    then evalStateT (loop envPrompt) Map.empty
    else pure mempty

main :: IO ()
main = do
  putText "Enter an expression in RPN (Reverse Polish Notation) and I will try to resolve it"
  expr <- getLine
  val <- getEnvVar
  printResult expr val
