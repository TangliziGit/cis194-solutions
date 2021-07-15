{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Calc where
import ExprT
import Parser
import StackVM
import Data.Either.Extra
import qualified Data.Map as Map

-- ex1
eval :: ExprT -> Integer
eval (ExprT.Lit x) = x
eval (ExprT.Add x y) = eval x + eval y
eval (ExprT.Mul x y) = eval x * eval y

-- ex2
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp ExprT.Lit ExprT.Add ExprT.Mul

-- ex3

-- `add` and `mul` function here is to reduce 2 expr into 1
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = ExprT.Add
  mul = ExprT.Mul

-- `id` function here is to convert `Expr a => a` into `ExprT`, something like explicit type casting
reify :: ExprT -> ExprT
reify = id

-- ex4
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
    lit x
      | x <= 0    = False
      | otherwise = True
    add = (||)
    mul = (&&)

-- newtype vs. data
-- the key point is that the construct for the newtype is guaranteed to be erased at compile time
-- so here MinMax can use (+), (*) operaters at runtime? No, unless it implements Num
newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = lit $ max x y
  mul (MinMax x) (MinMax y) = lit $ min x y

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 x) (Mod7 y) = lit $ x + y
  mul (Mod7 x) (Mod7 y) = lit $ x * y

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3*-4) + 5"

testInteger  = testExp :: Maybe Integer
testBool     = testExp :: Maybe Bool
testMM       = testExp :: Maybe MinMax
testSat      = testExp :: Maybe Mod7

-- ex5
instance Expr StackVM.Program where
  lit x = [ StackVM.PushI x ]
  add x y = x ++ y ++ [ StackVM.Add ]
  mul x y = x ++ y ++ [ StackVM.Mul ]

-- error handling like Rust
(?) :: (a -> Either b c) -> Either b a -> Either b c
(?) f (Right x) = f x
(?) f (Left x)  = Left x

run :: String -> Either String StackVal
run = (?) stackVM . maybeToEither "Parsing Error" . compile

compile :: String -> Maybe Program
compile = parseExp lit add mul

-- ex6
class HasVars a where
  var :: String -> a

-- TODO: difference between `data`, `newtype` and `type`?
type MapExpr = Map.Map String Integer -> Maybe Integer

instance HasVars MapExpr where
  var = Map.lookup

combineMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
combineMaybe f (Just a) (Just b) = Just $ f a b
combineMaybe _ _ _ = Nothing

instance Expr MapExpr where
  lit x = \_ -> Just x
  add fx fy = \m -> combineMaybe (+) (fx m) (fy m)
  mul fx fy = \m -> combineMaybe (*) (fx m) (fy m)

withVars :: [(String, Integer)] -> MapExpr -> Maybe Integer
withVars lis mapExpr = mapExpr $ Map.fromList lis
