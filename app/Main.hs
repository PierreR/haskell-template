{-# LANGUAGE OverloadedLists #-}
module Main where

-- import qualified Data.Text        as Text
-- import qualified Data.Text.IO     as Text
-- import qualified Data.Text.Lazy   as Text.Lazy
import qualified Data.Text.Read as Text
import Template.Math.RPN
import qualified Data.HashMap.Strict as Map

import Template.Prelude

printResult :: MonadIO io => Text -> EnvVars -> io ()
printResult expr env = either putStrLn print (evalRPN expr env)

getEnvVar :: IO EnvVars
getEnvVar = do
  putText "Need to set values ? (n or enter)"
  i <- getLine
  if (i == "n")
    then
      pure mempty
    else do
      putText "Variable name ?"
      var <- getLine
      putText "Value ?"
      val0 <- getLine
      let read_val = hush . second fst . Text.decimal
      case read_val val0 of
        Nothing -> do
          putText $  "Sorry cannot convert the value " <> val0 <> " into an integer"
          putText $ "Let's try again"
          getEnvVar
        Just v -> pure $ Map.singleton var v

main :: IO ()
main = do
  putText "Enter an expression in RPN (Reverse Polish Notation) and I will try to resolve it"
  expr <- getLine
  val <- getEnvVar
  printResult expr val
