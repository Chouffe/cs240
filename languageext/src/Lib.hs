{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo           #-}


module Lib where

import           Control.Monad
import           Control.Monad.Fix hiding (fix)
import           Control.Monad.Reader hiding (fix)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Mutual  recursion in where/let bindings
oneTwo :: (Int, Int)
oneTwo = (fst x, snd y)
    where x = (1, snd y)
          y = (fst x, 2)

nthFib :: Int -> Integer
nthFib n = fibList !! n
    where fibList = 1 : 1 : zipWith (+) fibList (tail fibList)

-- Recursion can be implemented using a fixed-point combinator

-- fix calls a function with its own result
fix :: (a -> a) -> a
fix f = let x = f x in x

oneTwo' :: (Int, Int)
oneTwo' = (fst y, snd x)
    where (x, y) = fix $ \ ~(x0, y0) -> let x1 = (1, snd y0)
                                            y1 = (fst x0, 2)
                                        in (x1, y1)

nthFib' :: Int -> Integer
nthFib' n = fibList !! n
    where fibList = fix $ \l -> 1 : 1 : zipWith (+) l (tail l)

-- Monadic binding are not recursive
-- do fibList <- return $ 1 : 1 : zipWith (+) fibList (tail fibList)
-- fibList not in scope...

-- Monads in the MonadFix class have fixed point combinator
--class Monad m => MonadFix m where
    -- mfix :: (a -> m a) -> m a

mfib :: (MonadFix m) => Int -> m Integer
mfib n = do
  fibList <- mfix $ \l -> return $ 1 : 1 : zipWith (+) l (tail l)
  return $ fibList !! n

-- RecursiveDo extension: introduces recursive bindings in a do block
-- Monad must be an instance of MonadFix (rec desugars to mfix calls)

oneTwo'' :: (MonadFix m) => m (Int, Int)
oneTwo'' = do
  rec x <- return (1, snd y)
      y <- return (fst x, 2)
  return (fst y, snd x)

-- Desugars to:
oneTwo''' :: (MonadFix m) => m (Int, Int)
oneTwo''' = do
  (x, y) <- mfix $ \ ~(x0, y0) -> do x1 <- return (1, snd y0)
                                     y1 <- return (fst x0, 2)
                                     return (x1, y1)
  return (fst y, snd x)

-- In practice, RecursiveDo helps structure thinking

-- Implementing mfix
newtype Identity a = Identity { runIdentity :: a }
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity $ f x

instance Monad Identity where
  return = Identity
  (Identity x) >>= f = f x

instance MonadFix Identity where
  mfix f = let x = f (runIdentity x) in x

instance Monad m => Functor (StateT s m) where
  fmap f x = StateT $ \s -> do
    (a, s1) <- runStateT x s
    return (f a, s1)

instance Monad m => Applicative (StateT s m) where
  pure x = StateT $ \s -> return (x, s)
  f <*> x = StateT $ \s0 -> do
    (g, s1) <- runStateT f s0
    (a, s2) <- runStateT x s1
    return (g a, s2)

instance Monad m => Monad (StateT s m) where
  return = pure
  x >>= f = StateT $ \s0 -> do
    (y, s1) <- runStateT x s0
    runStateT (f y) s1

instance MonadFix m => MonadFix (StateT s m) where
  -- mfix f = StateT $ \s0 -> do
  --   rec ~(a, s1) <- runStateT (f a) s0
  --   return (a, s1)
  mfix f = StateT $ \s0 -> mfix $ \ ~(a, _) -> runStateT (f a) s0
-- A generic mfix is not possible: >>= is strict in its first argument for many monads

class (Monad m) => MonadState s m where
  get :: m s
  put :: s -> m ()

instance (Monad m) => MonadState s (StateT s m) where
  get   = StateT $ \s -> return (s, s)
  put s = StateT $ \_ -> return ((), s)

instance (MonadState s m) => MonadState s (ReaderT r m) where
  get = lift get
  put = lift . put
