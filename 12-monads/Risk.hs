{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad
import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

-- ex2

updateBattle :: Ordering -> Battlefield -> Battlefield
updateBattle cmp field = case cmp of
  GT        -> field { defenders = (defenders field - 1) }
  otherwise -> field { attackers = (attackers field - 1) }

battle :: Battlefield -> Rand StdGen Battlefield
battle field = do
    atts <- replicateM maxAttackers die
    defs <- replicateM maxDefenders die
    return $ foldr updateBattle field $ zipWith compare (sort atts) (sort defs)
  where maxAttackers = min 3 (attackers field - 1)
        maxDefenders = min 2 (defenders field)

-- ex3
invade :: Battlefield -> Rand StdGen Battlefield
invade field@(Battlefield a d)
  | d == 2    = return field
  | a < 2     = return field
  | otherwise = battle field >>= invade

-- ex4
successProb :: Battlefield -> Rand StdGen Double
successProb field = do
  fs <- replicateM 1000 (invade field)
  return $ successCount fs
  where successCount = (/1000) . fromIntegral . length . filter win
        win (Battlefield _ 0) = True
        win otherwise         = False
