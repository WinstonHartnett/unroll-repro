{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import GHC.TypeLits
import qualified Data.Vector.Unboxed as V

-- Old definition
{-# INLINE incAll #-}
incAll :: V.Vector Int -> V.Vector Int
incAll = V.map (+ 1)

oldNTimes :: Int -> (a -> a) -> a -> a
oldNTimes 0 f x = x
oldNTimes n f x = f (oldNTimes (n-1) f x)

-- New definition
class Unroll (n :: Nat) where
  nTimes :: (a -> a) -> a -> a

instance Unroll 0 where
  nTimes f x = x

instance {-# OVERLAPPABLE #-} Unroll (p - 1) => Unroll p where
  nTimes f x = f (nTimes @(p - 1) f x)

main :: IO ()
main = do
  let size = 100000000 :: Int
  let array = V.replicate size 0 :: V.Vector Int
  print $ V.sum (nTimes @64 incAll array)
  -- print $ V.sum (oldNTimes 64 incAll array)

