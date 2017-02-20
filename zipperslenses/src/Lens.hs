module Lens where

import Control.Lens

tuple :: (String, String)
tuple = ("Hello", "World")

-- >>> tuple^._2       -> "World"
-- >>> set _2 42 tuple -> ("Hello", 42)
--
-- Composing lenses for reading/writing goes in the order an imperative language would

ntuple :: (String, (String, String))
ntuple = ("Hello", ("World", "!!!"))


-- >>> ntuple ^._2._1           -> "World"
-- >>> ntuple ^._2._2           -> "!!!
-- >>> set (_2._2) 42 ntuple    -> ("Hello",("World",42))
