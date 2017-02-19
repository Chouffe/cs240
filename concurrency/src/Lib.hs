{-# LANGUAGE DeriveDataTypeable #-}

module Lib where

import Control.Exception
import Data.Typeable
import Control.Monad
import Control.Concurrent
import System.IO.Error (isDoesNotExistError)
import Control.DeepSeq (deepseq)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Control.Concurrent.MVar
import Criterion.Main

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data MyError = MyError String
  deriving (Show, Typeable)

instance Exception MyError

catcher :: IO a -> IO (Maybe a)
catcher action = fmap Just action `catch` handler
  where handler (MyError msg) = putStrLn msg >> return Nothing

-- Examples
-- >>> catcher $ readFile "/dev/null"
-- >>> catcher $ throwIO $ MyError "Something Bad..."

-- Exceptions in pure code
-- Possible to throw exception in pure code, yet catch them only in IO

-- Non Deterministic (because LAZINESS)
myError :: Int
myError = 1 + (error "One") + (error "two")

exceptionIO :: IO Int
-- exceptionIO = do
--   x <- throwIO (MyError "one")
--   y <- throwIO (MyError "two")
--   return $ x + y
exceptionIO = liftM2 (+) (throwIO (MyError "one")) (throwIO (MyError "two"))

pureCatcher :: a -> IO (Maybe a)
pureCatcher a =
  (a `seq` return (Just a)) `catch` (\(SomeException _) -> return Nothing)

-- Problem with this code:
-- pureCatcher (undefined : undefined :: String)
-- Just "*** Exception: Prelude undefined
-- Because it only catches when thunks are actually evaluated!

-- Exercise: seqLike function that evaluates all elements in a list

seqList :: [a] -> b -> b
-- seqList [] b     = b
-- seqList (x:xs) b = seq x (seqList xs b)
seqList xs b = foldr (\x acc -> seq x acc) b xs

-- seqList [1,2,3] () == ()
-- seqList [1,2,undefined] _ == blows up

-- try :: Exception e => IO a -> IO (Either e a
-- finally :: IO a -> IO b -> IO a  -- clean up always
-- onException :: IO a -> IO b -> IO a -- after exception
-- catchJust :: Exception e => (e -> Maybe b) -> IO a -> (b -> IO a) -> IO a

readFileIfExists :: FilePath -> IO String
readFileIfExists f = catchJust p (readFile f) (\_ -> return "")
  where p e = if isDoesNotExistError e then Just e else Nothing

-- Concurrency
-- ------------------

-- Thread switch can happen any time GC could be invoked
-- forkIO :: IO () -> IO ThreadId  -- Creates a new thread
-- throwTo :: Exception e => ThreadId -> e -> IO ()
-- killThread :: ThreadId -> IO ()
-- threadDelay :: Int -> IO ()  -- sleeps for # od usec
-- myThreadId :: IO ThreadId


-- Example: Timeout
-- Execute IO action or abort after # of usec

data TimedOut = TimedOut UTCTime deriving (Eq, Show, Typeable)
instance Exception TimedOut

timeout :: Int -> IO a -> IO (Maybe a)
timeout usec action = do
  expired <- fmap TimedOut getCurrentTime
  ptid <- myThreadId
  let child = threadDelay usec >> throwTo ptid expired
  -- Bad if async IO exception... :(
  -- let parent = do ctid <- forkIO child
  --                 result <- action
  --                 killThread ctid
  --                 return $ Just result
  let parent = bracket (forkIO child) killThread $ \_ -> fmap Just action
  catchJust (\e -> if e == expired then Just e else Nothing)
            parent
            (\_ -> return Nothing)

-- Will timeout if more than 10 seconds
actionTimeout :: IO (Maybe String)
actionTimeout = do
  x <- timeout 10000000 getLine
  return x

-- MVar Type
-- Lets threads communicate via shared variables
-- An MVar t is a mutable variable of type t that is either full or empty

-- newEmptyMVar :: IO (MVar a)       -- creates an empty MVar
-- nemMVar :: a -> IO (MVar a)       -- Creates full MVar given value a
-- takeMVar :: MVar a -> IO a        -- Takes from the MVar, it is now empty, blocking
-- putMVar :: MVar a -> a -> IO ()   -- Puts value in an MVar, blocking

-- tryTakeMVar :: MVar a -> IO (Maybe a)  -- Nothing is empty, non blocking
-- tryPutMVar :: MVar a -> a -> IO Bool   -- False if full, non blocking

-- Example: pingpong

pingpong :: Bool -> Int -> IO ()
pingpong v n = do
  mvc <- newEmptyMVar   -- MVar read by child
  mvp <- newEmptyMVar   -- MVar read by parent
  let parent k | k > 0 = do when v $ putStr $ " " ++ show k
                            putMVar mvc k
                            takeMVar mvp >>= parent
               | otherwise = return ()
      child = do k <- takeMVar mvc
                 putMVar mvp (k - 1)
                 child
  tid <- forkIO child
  parent n `finally` killThread tid
  when v $ putStrLn ""

benchmarkPingPong :: IO ()
benchmarkPingPong = defaultMain [ bench "Thread switch test" (nfIO mybench) ]
  where mybench = pingpong False 100000
  -- where mybench = undefined

-- Working with MVars
-- MVars just work as a mutex
-- Mutex: Mutual Exclusion object
-- A Mutex allows multiple program threads to share the same resource (file access, socket, ...) but not simulteanously. Any thread that needs the resource must lock the mutex from other threads.

type Mutex = MVar ()

createMutex :: IO Mutex
createMutex = newMVar ()

lockMutex :: Mutex -> IO ()
lockMutex = takeMVar

unlockMutex :: Mutex -> IO ()
unlockMutex mv = putMVar mv ()

syncMutex :: Mutex -> IO a -> IO a
syncMutex mv action =
  bracket (lockMutex mv) (\_ -> unlockMutex mv) (\_ -> action)

-- Problem: Anyone can unlock a Mutex if it is locked
-- How to throw a failure if caller does not hol lock?
-- Just store threadId in MVar

type Mutex2 = MVar ThreadId

createMutex2 :: IO Mutex2
createMutex2 = newEmptyMVar

lockMutex2 :: Mutex2 -> IO ()
lockMutex2 mv = myThreadId >>= putMVar mv

unlockMutex2 :: Mutex2 -> IO ()
unlockMutex2 mv = do
  mytid <- myThreadId
  lockTid <- tryTakeMVar mv
  unless (lockTid == Just mytid) $ error "not possible to unlock with this thread!"

