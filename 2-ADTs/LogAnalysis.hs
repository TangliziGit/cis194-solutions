{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- ex1
parseMessageWords :: [String] -> LogMessage
parseMessageWords ("I":timestamp:rest) = LogMessage Info (read timestamp) (unwords rest)
parseMessageWords ("W":timestamp:rest) = LogMessage Warning (read timestamp) (unwords rest)
parseMessageWords ("E":errorCode:timestamp:rest) = LogMessage (Error (read errorCode)) (read timestamp) (unwords rest)
parseMessageWords = Unknown . unwords

parseMessage :: String -> LogMessage
parseMessage = parseMessageWords . words

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- ex2
insert :: LogMessage -> MessageTree -> MessageTree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ ta _) (Node left cur@(LogMessage _ tb _) right)
  | ta <= tb  = Node (insert msg left) cur right
  | otherwise = Node left cur (insert msg right)
insert _ tree = tree

-- ex3
build :: [LogMessage] -> MessageTree
build = foldl (\t x -> insert x t) Leaf

-- ex4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left cur right) = inOrder left ++ [cur] ++ inOrder right

-- ex5
isRelevant :: LogMessage -> Bool
isRelevant (LogMessage (Error e) _ _) = e >= 50
isRelevant _ = False

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ msg) = msg
getMessage (Unknown msg) = msg

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = (map getMessage) . (filter isRelevant)
