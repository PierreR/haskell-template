{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-binds -Wno-missing-local-signatures #-}

module Template.Lens.Playground ()
where

-- import qualified Data.Text        as Text
-- import qualified Data.Text.IO     as Text

import Template.Prelude

data Address = Address
  { _line1 :: Text
  , _country :: Text
  } deriving Show

data SkillLevel
  = Weak
  | Medium
  | Good
  | Excellent
  deriving (Show, Eq, Ord, Enum, Bounded)

data Person = Person
  { _name :: Text
  , _address :: Address
  , _skill :: [(Text, SkillLevel)]
  } deriving Show

makeClassy ''Person

paul = Person "Paul" (Address "15 rue du Sud" "Belgium") [("Gaming", Excellent), ("Cooking", Weak)]

succ' :: SkillLevel -> SkillLevel
succ' a  -- if a == maxBound then a else succ a
  | a == maxBound = a
  | otherwise = succ a

play0 = paul & skill.traverse._2 %~ succ'
play1 = paul ^..skill.traverse._1
