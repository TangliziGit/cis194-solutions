{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- ex1
first :: (a -> c) -> (a, b) -> (c, b)
first f t = (f $ fst t, snd t)

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> first f <$> p s

-- ex2
instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)

  Parser pa <*> Parser pb = Parser $ \str -> pa str
    >>= \(outA, strA) -> pb strA
    >>= \(outB, strB) -> Just (outA outB, strB)

-- ex3
abParser :: Parser (Char, Char)
abParser = builder <$> satisfy (=='a') <*> satisfy (=='b')
  where builder a b = (a, b)

abParser_ :: Parser ()
abParser_ = builder <$> satisfy (=='a') <*> satisfy (=='b')
  where builder a b = ()

intPair :: Parser [Integer]
intPair = builder <$> posInt <*> satisfy (==' ') <*> posInt
  where builder a ' ' b = [a, b]

-- ex4
instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  Parser a <|> Parser b = Parser $ \s -> a s <|> b s
  
-- ex5
intOrUppercase :: Parser ()
intOrUppercase = builder <$> (void posInt <|> void (satisfy isUpper))
  where builder _ = ()
        void p = (\_ -> ()) <$> p
