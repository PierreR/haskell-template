module Template.Math.RPN.PP where

import Data.Text.Prettyprint.Doc

data EvalError = NotEnoughElements | NotANumber Text | ExtraElements

instance Pretty EvalError where
  pretty (NotANumber err) = pretty $ "Can't convert " <> err <> " into an integer"
  pretty ExtraElements = pretty "The expression could not be fully resolved"
  pretty NotEnoughElements = pretty "Not enough elements in the expression"
