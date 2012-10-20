-----------------------------------------------------------------------------
-- |
-- Module      :  Polynomial.Basic
-- Copyright   :  (c) Matthew Donadio 2002
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Simple module for handling polynomials.
--
-----------------------------------------------------------------------------

-- TODO: We should really create a datatype for polynomials...

-- TODO: Should polydiv return the quotient and the remainder as a tuple?

module Polynomial.Basic where

-- * Types

-- | Polynomials are lists of numbers:
-- [ a0, a1, ... , an ] == an*x^n + ... + a1*x + a0
-- and negative exponents are currently verboten.

-- * Functions

-- | Evaluate a polynomial using Horner's method.

polyeval :: Num a => [a] -> a -> a
polyeval []     x = 0
polyeval (p:ps) x = p + x * polyeval ps x

-- | Add two polynomials

polyadd :: Num a => [a] -> [a] -> [a]
polyadd [] []          = []
polyadd [] ys          = ys
polyadd xs []          = xs
polyadd (x:xs) (y:ys)  = (x+y) : polyadd xs ys

-- | Subtract two polynomials

polysub :: Num a => [a] -> [a] -> [a]
polysub [] []          = []
polysub [] ys          = map negate ys
polysub xs []          = xs
polysub (x:xs) (y:ys)  = (x-y) : polysub xs ys

-- | Scale a polynomial

polyscale :: Num a => a -> [a] -> [a]
polyscale a x = map (a*) x

-- | Multiply two polynomials

polymult :: Num a => [a] -> [a] -> [a]
polymult (x:[]) ys = map (x*) ys
polymult (x:xs) ys = polyadd (map (x*) ys) (polymult xs (0:ys))

-- | Divide two polynomials

polydiv :: Fractional a => [a] -> [a] -> [a]
polydiv x y = reverse $ polydiv' (reverse x) (reverse y)
    where polydiv' (x:xs) y | length (x:xs) < length y = []
			    | otherwise = z : (polydiv' (tail (polysub (x:xs) (polymult [z] y))) y)
	      where z = x / head y

-- | Modulus of two polynomials (remainder of division)

polymod :: Fractional a => [a] -> [a] -> [a]
polymod x y = reverse $ polymod' (reverse x) (reverse y)
    where polymod' (x:xs) y | length (x:xs) < length y = (x:xs)
	                    | otherwise = polymod' (tail (polysub (x:xs) (polymult [z] y))) y
	      where z = x / head y

-- | Raise a polynomial to a non-negative integer power

polypow :: (Num a, Integral b) => [a] -> b -> [a]
polypow x 0 = [ 1 ]
polypow x 1 = x
polypow x 2 = polymult x x
polypow x n | even n = polymult x2 x2
	    | odd n  = polymult x (polymult x2 x2)
    where x2 = polypow x (n `div` 2)

-- | Polynomial substitution y(n) = x(w(n))

polysubst :: Num a => [a] -> [a] -> [a]
polysubst w x = foldr polyadd [0] (polysubst' 0 w x )
    where polysubst' _ _ []     = []
          polysubst' n w (x:xs) = map (x*) (polypow w n) : polysubst' (n+1) w xs

-- | Polynomial derivative

polyderiv :: Num a => [a] -> [a]
polyderiv (x:xs) = polyderiv' 1 xs
    where polyderiv' _ []     = []
          polyderiv' n (x:xs) = n * x : polyderiv' (n+1) xs

-- | Polynomial integration

polyinteg :: Fractional a => [a] -> a -> [a]
polyinteg x c = c : polyinteg' 1 x
    where polyinteg' _ []     = []
          polyinteg' n (x:xs) = x / n : polyinteg' (n+1) xs

-- | Convert roots to a polynomial

roots2poly :: Num a => [a] -> [a]
roots2poly (r:[]) = [-r, 1]
roots2poly (r:rs) = polymult [-r, 1] (roots2poly rs)
