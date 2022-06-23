{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

-- New monadic definition
{-# INLINE incAllM #-}
incAllM :: Monad m => V.Vector Int -> m (V.Vector Int)
incAllM = pure . V.map (+ 1)

class UnrollM (n :: Nat) where
  nTimesM :: Monad m => (a -> m a) -> a -> m a

instance UnrollM 0 where
  nTimesM f x = pure x

instance {-# OVERLAPPABLE #-} UnrollM (p - 1) => UnrollM p where
  nTimesM f x = f =<< nTimesM @(p - 1) f x

main :: IO ()
main = do
  let size = 100000000 :: Int
  let array = V.replicate size 0 :: V.Vector Int
  -- print $ V.sum (oldNTimes 64 incAll array)
  -- print $ V.sum (nTimes @64 incAll array)
  print . V.sum =<< nTimesM @64 incAllM array
