{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Encode (encodeChar)
import Test.HUnit (assertEqual, Assertion)
import Test.QuickCheck
import Data.Char (chr)
import System.Random

testASCII :: Assertion
testASCII =
  assertEqual "ASCII encodes as one code unit"
    1 (length (encodeChar 'a'))

badTest :: Assertion
badTest = do
  assertEqual "sestertius encodes as one code unit"
    1 (length (encodeChar '\x10198'))

-- Generalizing tests
testOne :: Char -> Assertion
testOne char = do
  assertEqual "ASCII encodes as one code unit"
    1 (length (encodeChar char))

testASCII' :: Assertion
testASCII' = mapM_ testOne ['\0'..'\127']

-- We get away here because Unicode is small and computers are fast
-- Lets randomize the test inputs

main :: IO ()
main = testSuite
-- main = do
--   putStrLn "Running Test Suite"
--   -- testASCII
--   testASCII'
--   -- badTest
--   putStrLn "End Test Suite"

propEncodeOne :: Char -> Bool
propEncodeOne c = length (encodeChar c) == 1

propEncodeOneBigChar :: BigChar -> Bool
propEncodeOneBigChar (Big c) = length (encodeChar c) == 1

testSuite :: IO ()
testSuite = quickCheck propEncodeOne >> quickCheck propEncodeOneBigChar

-- Unfortunately, the Arbitrary instance for Char only produces small chars
--
-- instance Arbitrary Char where
--   arbitrary = chr `fmap` oneof [choose (0,127),
--                                 choose (0,255)]

-- newtype BigChar = Big Char deriving (Eq, Show)
newtype BigChar = Big Char
  deriving (Eq, Show, Random)

instance Arbitrary BigChar where
  -- arbitrary = fmap (Big . chr) $ oneof [choose (0, 255), choose (0, 50000)]
  arbitrary = choose (Big '0', Big '\x10FFF')
