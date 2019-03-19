{-# LANGUAGE OverloadedStrings #-}

module Template.Math.Test
where

import qualified Data.HashMap.Strict       as Map
import           Data.Maybe                (fromJust)
import qualified Data.Text                 as Text
import           Data.Text.Prettyprint.Doc
import qualified Data.Text.Read            as Text
import           Template.Prelude          hiding (show)

type Stack = [Integer]
type ErrMsg = Text
type EvalM = ExceptT ErrMsg (State Stack)

push:: Integer -> EvalM ()
push x = modify (x:)

safeHead :: Stack -> EvalM Integer
safeHead []    = throwError "Not enough element in the stack"
safeHead (x:_) = pure x

pop :: EvalM Integer
pop = do
  xs <- get
  when (null xs) $
    throwError "Not enough element in the stack"
  put (tailSafe xs)
  pure $ fromJust (head xs)

handleNaN :: Text -> Maybe Integer -> EvalM Integer
handleNaN t Nothing  = throwError $ "Cannot convert " <> t <> " into a number"
handleNaN _ (Just x) = pure x

readInput :: Text -> Maybe Integer
readInput = hush . second fst . Text.decimal

processInput :: Text -> EvalM ()
processInput "*" = processOp (*)
processInput "+" = processOp (+)
processInput t   = handleNaN t (readInput t) >>= push

processOp op = op <$> pop <*> pop >>= push

eval :: Text -> Either ErrMsg Integer
eval input = evalState (runExceptT eval') []
 where
   eval' = traverse processInput (Text.words input) *> checkResult *>  pop

checkResult :: EvalM ()
checkResult = do
  xs <- get
  when (length xs /= 1) $
    throwError "Some elements have not been resolved"
