{-# LANGUAGE RankNTypes #-}
module Some
  ( module Data.Some
  , mapWithSome
  ) where

import Data.Some

mapWithSome :: [Some tag] -> (forall a. tag a -> b) -> [b]
mapWithSome xs fn = map (`withSome` fn) xs
