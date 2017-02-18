module Playground where

import Control.Monad
import Test.QuickCheck

data Tree a =
    Node (Tree a) (Tree a)
  | Leaf a
    deriving (Show)

-- instance (Arbitrary a) => Arbitrary (Tree a) where
--   arbitrary =
--     oneof [ liftM Leaf arbitrary
--           , liftM2 Node arbitrary arbitrary
--           ]

-- Problem with this definition because it can generate infinite Trees
randomTrees :: IO ()
randomTrees = sample (arbitrary :: (Gen (Tree Int)))

-- Safer instance
instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized tree

tree :: Arbitrary a => Int -> Gen (Tree a)
tree 0 = liftM Leaf arbitrary
tree n = oneof [ liftM Leaf arbitrary
               , liftM2 Node subtree subtree
               ]
  where subtree = tree (n `div` 2)
