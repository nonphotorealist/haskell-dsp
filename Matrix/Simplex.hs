-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Matrix.Simplex
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Two-step simplex algorithm
--
-- I only guarantee that this module wastes inodes
--
-----------------------------------------------------------------------------

-- Originally based off the code in Sedgewick, but modified to match the
-- conventions from Papadimitriou and Steiglitz.

-- TODO: Is our column/row selection the same as Bland's anti-cycle
-- algorithm?

-- TODO: Add check for redundant rows in two-phase algorithm

-- TODO: Lots of testing

module Matrix.Simplex (Simplex(..), simplex, twophase) where

import Data.Array

eps :: Double
eps = 1.0e-10

-------------------------------------------------------------------------------

-- Pivot around a!(p,q)

pivot :: Int -> Int -> Array (Int,Int) Double -> Array (Int,Int) Double
pivot p q a = step4 p q $ step3 p q $ step2 p q $ step1 p q $ a
    where step1 p q a = a // [ ((j,k), a!(j,k) - a!(p,k) * a!(j,q) / a!(p,q)) | k <- [0..m], j <- [ph..n], j /= p && k /= q ]
	  step2 p q a = a // [ ((j,q),0) | j <- [ph..n], j /= p ]
	  step3 p q a = a // [ ((p,k), a!(p,k) / a!(p,q)) | k <- [0..m], k /= q ]
	  step4 p q a = a // [ ((p,q),1) ]
	  ((ph,_),(n,m)) = bounds a

-- chooseq picks the lowest numbered favorable column.  If there are no
-- favorable columns, then q==m is returned, and we have reached an
-- optimum.

chooseq a = chooseq' 1 a
    where chooseq' q a | q > m          = q 
		       | a!(0,q) < -eps = q
		       | otherwise      = chooseq' (q+1) a
          ((_,_),(n,m)) = bounds a

-- choosep picks a row with a positive element in column q.  If no such
-- element exists, then the p==n is returned, and the problem is
-- unfeasible.

choosep q a = choosep' 1 q a
    where choosep' p q a | p > n         = p
			 | a!(p,q) > eps = p
			 | otherwise     = choosep' (p+1) q a
	  ((_,_),(n,m)) = bounds a

-- refinep picks the row using the ratio test.

refinep p q a = refinep' (p+1) p q a
    where refinep' i p q a | i > n = p
			   | a!(i,q) > eps && a!(i,0) / a!(i,q) < a!(p,0) / a!(p,q) = refinep' (i+1) i q a
		           | otherwise = refinep' (i+1) p q a
          ((_,_),(n,m)) = bounds a

-- * Types

-- | Type for results of the simplex algorithm

data Simplex a = Unbounded | Infeasible | Optimal a deriving (Read,Show)

gettab (Optimal a) = a

-- * Functions

-- | The simplex algorithm for standard form:
-- 
-- min   c'x
--
-- where Ax = b, x >= 0
--
-- a!(0,0) = -z
--
-- a!(0,j) = c'
--
-- a!(i,0) = b
--
-- a!(i,j) = A_ij

simplex :: Array (Int,Int) Double -- ^ stating tableau
	-> Simplex (Array (Int,Int) Double) -- ^ solution

simplex a | q > m      = Optimal a
	  | p > n      = Unbounded
          | otherwise  = simplex $ pivot p' q $ a
    where q = chooseq a
          p = choosep q a
          p' = refinep p q a
	  ((_,_),(n,m)) = bounds a

-------------------------------------------------------------------------------

addart a = array ((-1,0),(n,m+n)) $ z ++ xsi ++ b ++ art ++ x
    where z = ((-1,0), a!(0,0)) : [ ((-1,j),0) | j <- [1..n] ] ++ [ ((-1,j+n),a!(0,j)) | j <- [1..m] ]
	  xsi = ((0,0), -colsum a 0) : [ ((0,j),0) | j <- [1..n] ] ++ [ ((0,j+n), -colsum a j) | j <- [1..m] ]
	  b = [ ((i,0), a!(i,0)) | i <- [1..n] ]
	  art = [ ((i,j), if i == j then 1 else 0) | i <- [1..n], j <- [1..n] ]
	  x = [ ((i,j+n), a!(i,j)) | i <- [1..n], j <- [1..m] ]
          ((_,_),(n,m)) = bounds a

colsum a j = sum [ a!(i,j) | i <- [1..n] ]
    where ((_,_),(n,m)) = bounds a

delart a a' = array ((0,0),(n,m)) $ z ++ b ++ x
    where z = ((0,0), a'!(-1,0)) : [ ((0,j), a!(0,j)) | j <- [1..m] ]
	  b = [ ((i,0), a'!(i,0)) | i <- [1..n] ]
	  x = [ ((i,j), a'!(i,j+n)) | i <- [1..n], j <- [1..m] ]
          ((_,_),(n,m)) = bounds a

-- | The two-phase simplex algorithm

twophase :: Array (Int,Int) Double -- ^ stating tableau
	 -> Simplex (Array (Int,Int) Double) -- ^ solution

twophase a | cost a' > eps = Infeasible
           | otherwise     = simplex $ delart a (gettab a')
    where a' = simplex $ addart $ a

cost (Optimal a) = negate $ a!(0,0)
    where ((_,_),(n,m)) = bounds a

-------------------------------------------------------------------------------

{-

Test vectors

This is from Sedgewick

> x1 = listArray ((0,0),(5,8)) [  0, -1, -1, -1, 0, 0, 0, 0, 0,
>	 		          5, -1,  1,  0, 1, 0, 0, 0, 0,
>			         45,  1,  4,  0, 0, 1, 0, 0, 0,
>			         27,  2,  1,  0, 0, 0, 1, 0, 0,
>			         24,  3, -4,  0, 0, 0, 0, 1, 0,
>			          4,  0,  0,  1, 0, 0, 0, 0, 1 ] :: Array (Int,Int) Double

P&S, Example 2.6

> x2 = listArray ((0,0),(3,5)) [ 0, 1, 1, 1, 1, 1,
>		                 1, 3, 2, 1, 0, 0,
>		                 3, 5, 1, 1, 1, 0,
>		                 4, 2, 5, 1, 0, 1 ] :: Array (Int,Int) Double

P&S, Example 2.6 (after BFS selection)

> x2' = listArray ((0,0),(3,5)) [ -6, -3, -3,  0,  0,  0,
>			          1,  3,  2,  1,  0,  0,
>			          2,  2, -1,  0,  1,  0,
>			          3, -1,  3,  0,  0,  1 ] :: Array (Int,Int) Double

P&S, Example 2.2 / Section 2.9

> x3 = listArray ((0,0),(4,7)) [ -34, -1, -14, -6, 0, 0, 0, 0,
>	                           4,  1,   1,  1, 1, 0, 0, 0,
>		                   2,  1,   0,  0, 0, 1, 0, 0,
>		                   3,  0,   0,  1, 0, 0, 1, 0,
>		                   6,  0,   3,  1, 0, 0, 0, 1 ] :: Array (Int,Int) Double

P&S, Example 2.7

> x4 = listArray ((0,0),(3,7)) [ 3, -3/4,  20, -1/2, 6, 0, 0, 0,
>		                 0,  1/4,  -8,   -1, 9, 1, 0, 0,
>		                 0,  1/2, -12, -1/2, 3, 0, 1, 0, 
>		                 1,    0,   0,    1, 0, 0, 0, 1 ] :: Array (Int,Int) Double

These come in handy for testing

> row j a = listArray (0,m) [ a!(j,k) | k <- [0..m] ]
>    where ((_,_),(n,m)) = bounds a

> column k a = listArray (0,n) [ a!(j,k) | j <- [0..n] ]
>    where ((_,_),(n,m)) = bounds a

> solution (Optimal a) = listArray (1,m) $ [ find a j | j <- [1..m] ]
>    where ((_,_),(n,m)) = bounds a

> find a j = findone' a 1 j
>     where findone' a i j | i > n          = 0
>	                   | a!(i,j) == 1.0 = b!i
>		           | otherwise      = findone' a (i+1) j
>           b = listArray (1,n) [ a!(i,0) | i <- [1..n] ]
>           ((_,_),(n,m)) = bounds a

-}
