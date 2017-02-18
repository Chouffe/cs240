module Lib where

import System.IO
import Network

someFunc :: IO ()
someFunc = putStrLn "someFunc"

factorial :: Integer -> Integer
factorial n = go 1 n
  where go acc k
          | k <= 1 = acc
          | otherwise = go (acc * k) (k - 1)

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
withClient portID fn = do
  s <- listenOn portID
  (h, host, port) <- accept s
  putStrLn $ "Connection from host " ++ host ++ " port " ++ show port
  sClose s  -- Only accept one client
  a <- fn h
  hClose h  -- Close handle
  return a
