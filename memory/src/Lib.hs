module Lib where

import Control.Concurrent
import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TVar

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- First try with MVars
type Account = MVar Double

transfer :: Double -> Account -> Account -> IO ()
transfer amount from to =
  modifyMVar_ from (\fromAmount -> do
      when (fromAmount < amount) $ fail "Not enough fund..."
      (modifyMVar_ to (\toAmount -> return (toAmount + amount)))
      return (fromAmount - amount))

transferAction :: IO ()
transferAction = do
  from <- newMVar 15
  to <- newMVar 10
  putStrLn "Transfering..."
  transfer 10 from to
  putStrLn "Transfer done: "
  newFromAmount <- readMVar from
  newToAmount <- readMVar to
  putStrLn $ "From amount: " ++ show newFromAmount
  putStrLn $ "To amount: " ++ show newToAmount

deadlock :: IO ()
deadlock = do
  from <- newMVar 15
  to <- newMVar 10
  forkIO $ transfer 1 from to
  forkIO $ transfer 1 to from
  newFromAmount <- readMVar from
  newToAmount <- readMVar to
  putStrLn $ "From amount: " ++ show newFromAmount
  putStrLn $ "To amount: " ++ show newToAmount
  putStrLn "Can deadlock when transferring money in both directions"

-- One solution: use non blocking tryTakeMVar for second MVar
-- if it fails, release both and try again in different order
-- This is gross.... :(

transfer2 :: Double -> Account -> Account -> IO ()
transfer2 amount from to = do
  let tryTransfer = modifyMVar from $ \ fromAmount -> do
        when (fromAmount < amount) $ fail "Not enough money..."
        mbt <- tryTakeMVar to
        case mbt of
          Just bt -> (putMVar to $! bt + amount) >> return (fromAmount - amount, True)
          Nothing -> return (fromAmount, False)
  ok <- tryTransfer
  unless ok $ transfer2 (- amount) to from

-- This is horrible code because MVars are not suited for database-like transactions...
-- Read/Write a bunch of variables
-- Commit atomically at end
-- It is really hard to do in C/Java: externalized actions cant easily be rolled back
-- But in Haskell, the IO type can control side effect


-- STM basics
--
-- newTVarIO :: a -> IO (TVar a)
-- readTVarIO :: TVar a -> IO a
-- readTVar :: TVar a -> STM a
-- writeTVar :: Tvar a -> a -> STM ()
-- modifyTVar :: TVar a -> (a -> a) -> STM ()  -- lazy
-- modifyTVar' :: TVar a -> (a -> a) -> STM () -- strict

-- STM monad allows TVar access but not irreversible side effects
-- atomically :: STM a -> IO a
-- Lets you run STM computations from IO
-- Semantics of on global lock + parallelism of fine grained locks
-- Tradeoff: Give up the ability to perform externalized IO actions
