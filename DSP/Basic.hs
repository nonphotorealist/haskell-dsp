-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Basic
-- Copyright   :  (c) Matthew Donadio 1998
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Basic functions for manipulating signals
--
-----------------------------------------------------------------------------

module DSP.Basic where

import Data.Array

import DSP.Source.Basic

-- * Functions

-- | @z@ is the unit delay function, eg,
--
-- @z [ 1, 2, 3 ] == [ 0, 1, 2, 3 ]@

z  :: (Num a) => [a] -> [a]
z a = 0 : a

-- | zn is the n sample delay function, eg,
-- 
-- @zn 3 [ 1, 2, 3 ] == [ 0, 0, 0, 1, 2, 3 ]@

zn    :: (Num a) => Int -> [a] -> [a]
zn 0 a = a
zn n a = 0 : zn (n - 1) a

-- | @downsample@ throws away every n'th sample, eg,
--
-- @downsample 2 [ 1, 2, 3, 4, 5, 6 ] == [ 1, 3, 5 ]@

downsample :: (Num a) => Int -> [a] -> [a]
downsample n []     = []
downsample n (x:xs) = x : downsample n (drop (n - 1) xs)

-- | @upsample@ inserts n-1 zeros between each sample, eg,
-- 
-- @upsample 2 [ 1, 2, 3 ] == [ 1, 0, 2, 0, 3, 0 ]@

upsample :: (Num a) => Int -> [a] -> [a]
upsample _ []     = []
upsample n (x:xs) = x : zero n n xs
    where zero n 1 xs = upsample n xs
	  zero n i xs = 0 : zero n (i-1) xs

-- | @upsampleAndHold@ replicates each sample n times, eg,
--
-- @upsampleAndHold 3 [ 1, 2, 3 ] == [ 1, 1, 1, 2, 2, 2, 3, 3, 3 ]@

upsampleAndHold :: (Num a) => Int -> [a] -> [a]
upsampleAndHold n xs = hold' n n xs
    where hold' _ _ []     = []
	  hold' n 1 (x:xs) = x : hold' n n xs
	  hold' n i (x:xs) = x : hold' n (i-1) (x:xs)

-- | pad a sequence with zeros to length n
--
-- @pad [ 1, 2, 3 ] 6 == [ 1, 2, 3, 0, 0, 0 ]@

pad :: (Ix a, Integral a, Num b) => Array a b -> a -> Array a b
pad x n = listArray (0,n-1) $ elems x ++ zeros
