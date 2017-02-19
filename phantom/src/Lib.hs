{-# LANGUAGE FlexibleInstances #-}

module Lib
  ( Part(..)
  , Payload
  , param
  , filePart
  , fileString
  , Ref  -- Export type constructor by not data constructor
  , newRef
  , readOnly
  , readRef
  , writeRef
  , someFunc
  )
  where

import Data.Monoid
import Data.IORef

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype FirstRight a b = FirstRight { getFirstRight :: Either a b }

instance Monoid (FirstRight String a) where
  mempty = FirstRight $ Left "fkdjfal"

  mappend a@(FirstRight (Right _)) _ = a
  mappend _ b = b

-- Phantom types and Monoids

type Param = (String, String)
type ContentType = String

-- Phantom Type a, does not appear in RHS
data Payload a =
    NoPayload
  | Raw ContentType String
  | Params [Param]
  | FormData [Part]
  deriving (Show)

data Part = Part {
    -- name of the <input> tag this belongs to
    name          :: String
    -- filename of file we're uploading
    , fileName    :: Maybe FilePath
    -- type of file
    , contentType :: Maybe ContentType
    -- file contents
    , body        :: String
    }
  deriving (Show)

-- Lets try to write a Monoid Instance

-- instance Monoid (Maybe Payload) where
--   mempty = Nothing

--   mappend Nothing b = b
--   mappend a Nothing = a
--   mappend (Just (Params a)) (Just (Params b)) = Just (Params (a ++ b))
--   mappend (Just (FormData a)) (Just (FormData b)) = Just (FormData (a ++ b))
--   mappend _ _ = Nothing
  -- Crappy because everytime we use mappend, we have to pattern match the result to see if the mappend succeeded
  -- Other issue: Data.Monoid already defines an instance of Monoid for Maybe a
  -- FlexibleInstances allows these two definitions to coexist happily
  -- But when we want to use an instance, ghci does not know which one to use.
  -- OverlappingInstances pragma allows multiple instances to coexist.
  -- This is still bad...
  -- makes it easy for incorrect programs to still typecheck
  -- can cause confusing error messages
  -- The solution is to use Phantom Types!!

-- Public API for creating Payload values
-- We need to export the name of type Part, but not any of its constructors
-- -- module PayloadPhantom
--     (
--       Part(..)
--     , Payload -- no constructors
--     , param
--     , filePart
--     , fileString
--     {- ... trimmed out ... -}
--     ) where

-- Returns a Payload [Param]
param :: String -> String -> Payload [Param]
param name value = Params [(name, value)]

-- Returns a Payload [Part]
filePart :: String -> FilePath -> IO (Payload [Part])
filePart name path = do
  body <- readFile name
  return (FormData [Part name (Just path) Nothing body])

-- The Phantom Type makes these distinct types
-- The runtime representation is the same in each case
-- The compiler prevents us from mixing the two by accident

fileString :: String -> Maybe FilePath -> String -> (Payload [Part])
fileString n f b = FormData [Part n f (Just "string") b]

-- Now it works for combining two Payload [Param]
instance Monoid (Payload [Param]) where
  mempty = NoPayload
  mappend NoPayload a = a
  mappend a NoPayload = a
  mappend (Params a) (Params b) = Params $ a ++ b

instance Monoid (Payload [Part]) where
  mempty = NoPayload
  mappend NoPayload a = a
  mappend a NoPayload = a
  mappend (FormData a) (FormData b) = (FormData (a ++ b))

-- param "foo" "bar" <> param "baz" "quux"
-- param "foo" "bar" <> fileString "baz" Nothing "quux"  -- will cause an compiler error

-- Monoids are awesome!
-- Simple
-- Easy for clients to use
-- Force you to address API design problems early on
-- Phantom Types make it easier to build APIs where flat out broken behaviours can be made impossible by the compiler

-- Mutable Variables
-- Data.IORef

-- Create a new type of mutable reference
-- We use a phantom type t to statically track whether a piece of code is allowed to modify the reference or not
--
newtype Ref t a = Ref (IORef a)

data ReadOnly
data ReadWrite

newRef :: a -> IO (Ref ReadWrite a)
newRef a = fmap Ref $ newIORef a

readRef :: Ref t a -> IO a
readRef (Ref ref) = readIORef ref

writeRef :: Ref ReadWrite a -> a -> IO ()
writeRef (Ref ref) v = writeIORef ref v

readOnly :: Ref t a -> Ref ReadOnly a
readOnly (Ref ref) = Ref ref

-- We do not provide a function that can promote a ReadOnly to a ReadWrite
-- We only export the public API
