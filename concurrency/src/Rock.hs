module Rock where


import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Typeable
import Network
import System.IO

-- Small Rock/Paper/Scissors

data Move = Rock | Paper | Scissors
  deriving (Eq, Read, Show, Enum, Bounded)

data Outcome = Lose | Tie | Win
  deriving (Show, Eq, Ord)

outcome :: Move -> Move -> Outcome
outcome Rock Paper     = Lose
outcome Paper Scissors = Lose
outcome Scissors Rock  = Lose
outcome m1 m2
  | m1 == m2 = Tie
  | otherwise = Win

parseMove :: String -> Maybe Move
parseMove s =
  case reads s of
    [(m, "")] -> Just m
    _ -> Nothing

parseMove' :: String -> Maybe Move
parseMove' s =
  case reads s of
    [(m, rest)] | valid rest -> Just m
    _                        -> Nothing
  where valid = all (`elem` " \r\n")

greet :: Handle -> IO ()
greet h = do
  hPutStrLn h "What is your name?"
  name <- hGetLine h
  hPutStrLn h $ "Hi, " ++ name

withTty :: (Handle -> IO a) -> IO a
withTty = withFile "/dev/tty" ReadWriteMode

computerVsUser :: Move -> Handle -> IO ()
computerVsUser computerMove h = do
  hPutStrLn h $ "Please enter one of " ++ show ([minBound..] :: [Move])
  userInput <- hGetLine h
  case parseMove' userInput of
    Nothing       -> computerVsUser computerMove h
    Just userMove ->
      case outcome userMove computerMove of
        Win -> hPutStrLn h "You Win"
        Lose -> hPutStrLn h "You Lose"
        Tie -> hPutStrLn h "Tie..."  >> computerVsUser computerMove h

data Point = Point { x :: !Double, y :: !Double }
  deriving (Eq, Show)

myPoint :: Point
myPoint = Point 1 2

-- Networking
-- connectTo :: Hostname -> PortID -> IO Handle
-- listenOn :: PortID -> IO Socket
-- sClose :: Socket -> IO ()
-- hClose :: Handle -> IO ()

withClient :: PortID -> (Handle -> IO a) -> IO a
-- withClient portID fn = do
--   s <- listenOn portID
--   (h, host, port) <- accept s
--   putStrLn $ "Connection from host " ++ host ++ " port " ++ show port
--   sClose s  -- Only accept one client
--   a <- fn h
--   hClose h  -- Close handle
--   return a
-- withClient portID fn = do
--   s <- listenOn portID
--   bracket (fmap first (accept s)) (\h -> sClose s >> hClose h) fn
-- first :: (a, b, c) -> a
-- first (a, _, _) = a

withClient portID fn =
  bracket (listenOn portID) sClose $ \s -> do
    bracket (accept s) (\(h, _, _) -> hClose h) $
      \(h, host, port) -> do
        putStrLn $ "Connection from host " ++ host ++ " port " ++ show port
        fn h

getPlayerMove :: Handle -> IO Move
getPlayerMove h = do
  hPutStrLn h $ "Please enter one of " ++ show ([minBound..] :: [Move])
  userInput <- hGetLine h
  case parseMove' userInput of
    Nothing       -> getPlayerMove h
    Just userMove -> return userMove

netRock :: PortID -> IO ()
netRock portID = do
  player1MVar <- newEmptyMVar
  player2MVar <- newEmptyMVar
  bracket (listenOn portID) sClose $ \s -> do
    bracket (accept s) (\(h, _, _) ->  hClose h) $
      \(h1, host1, port1) -> do
          hPutStrLn h1 "Please wait for an opponent..."
          bracket (accept s) (\(h, _, _) -> hClose h) $
            \(h2, host2, port2) -> do
              tid1 <- forkIO $ getPlayerMove h1 >>= putMVar player1MVar
              tid2 <- forkIO $ getPlayerMove h2 >>= putMVar player2MVar
              play player1MVar player2MVar (h1, host1, port1)
              play player2MVar player1MVar (h2, host2, port2)
              killThread tid1
              killThread tid2

play :: MVar Move -> MVar Move -> (Handle, HostName, PortNumber) -> IO ()
play moveMvar1 moveMvar2 (h, host, port) = do
  putStrLn $ "Connection from host " ++ host ++ " port " ++ show port
  move1 <- takeMVar moveMvar1
  move2 <- takeMVar moveMvar2
  putMVar moveMvar1 move1
  putMVar moveMvar2 move2
  case outcome move1 move2 of
    Win -> hPutStrLn h "You Win"
    Lose -> hPutStrLn h "You Lose"
    Tie -> hPutStrLn h "Tie..."
