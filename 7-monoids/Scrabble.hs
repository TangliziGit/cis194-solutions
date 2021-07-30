module Scrabble where

import Data.Char

data Score = Score Int
  deriving (Show, Eq)

instance Semigroup Score where
  (<>) (Score a) (Score b) = Score $ a + b

instance Monoid Score where
  mempty = Score 0
  mappend = (<>) 

score :: Char -> Score
score ch
  | ch' `elem` "aeilnorstu" = Score 1
  | ch' `elem` "dg"         = Score 2
  | ch' `elem` "bcmp"       = Score 3
  | ch' `elem` "fhvwy"      = Score 4
  | ch' `elem` "k"          = Score 5
  | ch' `elem` "jx"         = Score 8
  | ch' `elem` "qz"         = Score 10
  | otherwise              = Score 0
  where ch' = toLower ch

scoreString :: String -> Score
scoreString = foldr combine (Score 0)
  where combine = (\x s -> s <> score x)

getScore :: Score -> Int
getScore (Score x) = x
