-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Random.Distribution.Poisson
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- UNTESTED
--
-- Module for transforming a list of uniform random variables into a
-- list of Poisson random variables.
--
-- Reference: Ross
--
----------------------------------------------------------------------------

module Numeric.Random.Distribution.Poisson (poisson) where

-- * Functions

-- | Generates a list of poisson random variables from a list
-- of uniforms

poisson :: Double    -- ^ lambda
	-> [Double]  -- ^ U
	-> [Double]  -- ^ X
	      
poisson lambda (u:us) = poisson' u us
    where poisson' n (u:us) | n < e     = n-1 : poisson lambda (u:us)
			    | otherwise = poisson' (n*u) us
	  e = exp (-lambda)
