module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Zippers and Lenses

-- Zero valued type: for programming with types while compiling
-- Used in phantom types for example
data Void
data Empty

data A = A Bool
       | B Ordering

-- Accessing nested data is easy
-- Updating nested data is hard and does not compose...

-- setAddrZip :: Zip -> Address -> Address
-- setAddrZip zip addr = addr { addrZip = zip }
-- Means:
--      Make a complete copy of the record addr
--      When copying, set the addrZip field to zip
-- Its a way of editing a value that leaves the original unchanged but does not require us to specify every field to copy
-- It is a very non-composable hack...
--
-- What to do?

editSnd :: (b -> c) -> (a, b) -> (a, c)
editSnd f (a, b) = (a , f b)

editFst :: (a -> c) -> (a, b) -> (c, b)
editFst f (a, b) = (f a, b)

data PairHole a b = HoleFst b
                  | HoleSnd a
  deriving (Eq, Show)

data PairZipper a b c = PZ c (PairHole a b)
  deriving (Eq, Show)

focusFst :: (a, b) -> PairZipper a b a
focusFst (a, b) = PZ a (HoleFst b)

focusSnd :: (a, b) -> PairZipper a b b
focusSnd (a, b) = PZ b (HoleSnd a)

unfocusFst :: PairZipper a b a -> (a, b)
unfocusFst (PZ a (HoleFst b)) = (a, b)

unfocusSnd :: PairZipper a b b -> (a, b)
unfocusSnd (PZ b (HoleSnd a)) = (a, b)

view1 :: PairZipper a b c -> c
view1 (PZ c _) = c

over1 :: (c -> d) -> PairZipper a b c -> PairZipper a b d
over1 f (PZ c l) = PZ (f c) l

-- This approach has problems
-- we have to specify what field we are focusing at both ends of the pipeline
-- we cannot compose focusFst and focusSnd to get another zipper

data Focused t a b = Focused {
    focused :: a
  , rebuild :: b -> t
  }

-- I am focusing on an a
-- I might change the type to b
-- When I am done, I will give you back a t

-- A pair made of
--      The focused element
--      A function that knows how to reconstitute the original value

type Focuser s t a b = s -> Focused t a b

-- give me an s
-- I will focus an a
-- I might change its type to b
-- When I am done focusing, I might change the thing I give you back from s to t

-- A Focuser is a function that takes a value and gives us a Focused
-- It is very polymorphic, `over` wasnt polymorphic enough

unfocus :: Focused s a a -> s
unfocus (Focused focused rebuild) = rebuild focused

view :: Focuser s t a b -> s -> a
view l s = focused (l s)

over :: Focuser s t a b -> (a -> b) -> s -> t
over l f s = let Focused focused rebuild = l s
             in rebuild (f focused)


_1 :: Focuser (a,b) (c,b) a c
_1 (a,b) = Focused a (\c -> (c,b))

_2 :: Focuser (a,b) (a,c) b c
_2 (a,b) = Focused b (\c -> (a,c))

focusHead :: Focuser [a] [a] a a
focusHead []     = error "Empty list"
focusHead (x:xs) = Focused x (\s -> (s:xs))
