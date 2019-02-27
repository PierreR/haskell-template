{-# LANGUAGE OverloadedLists #-}
module Main where

import qualified Data.HashMap.Strict                   as Map
import           Data.Text.Prettyprint.Doc             (pretty)
import           Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import qualified Data.Text.Read                        as Text
import           Template.Math.RPN                     (EnvVars)
import qualified Template.Math.RPN                     as RPN

import           Template.Prelude

printResult :: Text -> EnvVars -> IO ()
printResult expr env = either (putDoc . pretty) print (RPN.evalRPN expr env)

getEnvVar :: IO EnvVars
getEnvVar = do
  putText "Need to set values ? (y or enter to ignore)"
  i <- getLine
  if i == "y"
    then do
      (k,v) <- askVar
      pure $ Map.singleton k v
    else pure mempty

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

main :: IO ()
main = do
  putText "Enter an expression in RPN (Reverse Polish Notation) and I will try to resolve it"
  expr <- getLine
  val <- getEnvVar
  printResult expr val
