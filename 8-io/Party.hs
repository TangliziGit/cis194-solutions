{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Data.List
import Data.Tree
import Data.Foldable

import Employee

-- ex1
glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps fun) = GL (emp : emps) $ fun + empFun emp

instance Semigroup GuestList where
  (<>) (GL empsA funA) (GL empsB funB) = GL (empsA ++ empsB) (funA + funB)

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- ex2
-- here use `[b]` instead of `b`, because we want users to deal with the node val and the result of all subNodes
-- `<$>` is `fmap` or `>>=`
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node val subs) = f val (treeFold f <$> subs)

-- ex3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss glTuples = (glCons boss withoutBosses, withBosses)
  where withoutBosses = foldMap snd glTuples
        withBosses    = foldMap (uncurry moreFun) glTuples

-- ex4
maxFun :: Tree Employee -> GuestList
maxFun tree = uncurry moreFun $ treeFold nextLevel tree

maxFunTest :: Bool
maxFunTest = and 
  [
    GL [tom] 8 == maxFun (Node tom []),
    GL [tom] 8 == maxFun (Node bob [Node tom []]),
    GL [joe, bob] 10 == maxFun (Node tom [Node joe [], Node bob []]),
    GL [joe, joe] 18 == maxFun caseA
  ]
    where tom = Emp "Tom" 8
          bob = Emp "Bob" 1
          joe = Emp "Joe" 9
          caseA = Node tom [
                      Node tom [
                          Node joe []
                      ],
                      Node joe []
                  ]

-- ex5
glFormat :: GuestList -> String
glFormat (GL emps fun) = "Total fun: " ++ show fun ++ "\n" ++ empsFormat emps
  where empsFormat = unlines . sort . map empName

main :: IO ()
main = readFile "company.txt" 
  >>= putStrLn . glFormat . maxFun . read
