module AccountStm where

import Control.Concurrent
import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TVar

type Account = TVar Double

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

transfer :: Double -> Account -> Account -> STM ()
transfer amount from to = do
  modifyTVar' from (subtract amount)
  modifyTVar' to (+ amount)

transferAction :: IO ()
transferAction = do
  from <- newTVarIO 10
  to <- newTVarIO 5
  atomically $ transfer 1 from to
  fromAmount <- readTVarIO from
  toAmount <- readTVarIO to
  putStrLn $ "From amount: " ++ show fromAmount
  putStrLn $ "To amount: " ++ show toAmount

-- What if you want to wait when not enough money in account?
-- retry :: STM a
-- retry aborts the transaction, can read changes without explicit condition variables

transfer' :: Double -> Account -> Account -> STM ()
transfer' amount from to = do
  bf <- readTVar from
  when (amount > bf) retry
  modifyTVar' from (subtract amount)
  modifyTVar' to (+ amount)

-- orElse :: STM a -> STM a -> STM a
-- orElse tries second action if first one aborts (sleeps if both abort)
-- Provides nested transactions

transfer'' :: Double -> Account -> Account -> Account -> STM ()
transfer'' amount from1 from2 to =
  transfer amount from1 to `orElse` transfer amount from2 to

-- Enforcing invariants
-- alwaysSucceeds :: STM a -> STM ()
-- adds invariant to check after every transaction (Either the invariant throws an exception or its return value is ignored)
-- Eg. No negative account balances ever

newAccount :: Double -> STM Account
newAccount balance = do
  tvar <- newTVar balance
  alwaysSucceeds $ do
    balance <- readTVar tvar
    when (balance < 0) $ fail "negative balance"
  return tvar

bogus :: IO ()
bogus = do
  account <- atomically $ newAccount 10
  atomically $ modifyTVar account (subtract 15)
