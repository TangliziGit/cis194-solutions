{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module JoinList where

import Data.Monoid
import Debug.Trace

import Scrabble
import Buffer
import Editor
import Sized

-- ex1
data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b

joinListToList :: JoinList m a -> [a]
joinListToList Empty = []
joinListToList (Single _ a) = [a]
joinListToList (Append _ as bs) = joinListToList as ++ joinListToList bs

-- ex2
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ idx (Single _ x)
  | idx == 0  = Just x
  | otherwise = Nothing
indexJ idx (Append m left right)
  | idx < leftSize  = indexJ idx left
  | idx < wholeSize = indexJ (idx - leftSize) right
  | otherwise       = Nothing
  where wholeSize = getSize (size m)
        leftSize  = getSize . size $ tag left
  
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n lst
  | n <= 0 = lst
dropJ n Empty = Empty
dropJ n (Single _ _) = Empty
dropJ n (Append m left right)
  | n < leftSize  = Append (tag newLeft <> tag right) newLeft right
  | n < wholeSize = newRight
  | otherwise     = Empty
  where wholeSize = getSize (size m)
        leftSize  = getSize . size $ tag left
        newLeft   = dropJ n left
        newRight  = dropJ (n - leftSize) right

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n _
  | n <= 0 = Empty
takeJ n Empty = Empty
takeJ n lst@(Single _ _) = lst
takeJ n lst@(Append m left right)
  | n < leftSize  = takeJ n left
  | n == leftSize = left
  | n < wholeSize = Append (tag left <> tag newRight) left newRight
  | otherwise     = lst
  where wholeSize = getSize (size m)
        leftSize  = getSize . size $ tag left
        newRight  = takeJ (n - leftSize) right


caseA :: JoinList Size String
caseA = Append (Size 3)
          (Append (Size 2)
            (Append (Size 1)
              (Single (Size 1) "hello")
              (Empty))
            (Single (Size 1) "haskell"))
          (Single (Size 1) "!")

-- ex3
scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

-- ex4
type JoinListBuffer = JoinList (Score, Size) String

instance Buffer (JoinList (Score, Size) String) where
  toString          = unlines . joinListToList

  fromString        = foldr combine Empty . lines
    where combine = \l lst -> Single (scoreString l, 1) l +++ lst

  line              = indexJ

  replaceLine n l b = takeJ (n-1) b +++ fromString l +++ dropJ n b

  numLines          = getSize . snd . tag

  value             = getScore . fst . tag


main = runEditor editor input
  where input = fromString text :: JoinListBuffer
        text  = unlines
                  [ "This buffer is for notes you don't want to save, and for"
                  , "evaluation of steam valve coefficients."
                  , "To load a different file, type the character L followed"
                  , "by the name of the file."
                  ]
  
