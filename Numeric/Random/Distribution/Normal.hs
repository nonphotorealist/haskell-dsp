-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Random.Distribution.Normal
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Module for transforming a list of uniform random variables into a
-- list of normal random variables.
--
-----------------------------------------------------------------------------

-- TODO: The speedup from Ross for the A-R method

-- TODO: Marsaglia's ziggurat method

-- TODO: Leva' method

-- TODO: Ahrens-Dieter method

module Numeric.Random.Distribution.Normal (normal_clt, normal_bm, 
					   normal_ar, normal_r) where

-- * Functions

-- adjust takes a unit normal random variable and sets the mean and
-- variance to whatever is needed.

adjust :: (Double,Double) -> Double -> Double
adjust (mu,sigma) x = mu + sigma * x

-- | Normal random variables via the Central Limit Theorm (not explicity
-- given, but see Ross)
--
-- If mu=0 and sigma=1, then this will generate numbers in the range
-- [-n/2,n/2]

normal_clt :: Int             -- ^ Number of uniforms to sum
	   -> (Double,Double) -- ^ (mu,sigma)
	   -> [Double]        -- ^ U
	   -> [Double]        -- ^ X

normal_clt n (mu,sigma) u = map (adjust (mu,sigma)) $ normal' u
    where normal' us = var_adj * ((sum $ take n us) - mean_adj) : (normal' $ drop n us)
	  var_adj  = sqrt $ 12 / fromIntegral n
	  mean_adj = fromIntegral n / 2

-- | Normal random variables via the Box-Mueller Polar Method (Ross, pp
-- 450--452)
-- 
-- If mu=0 and sigma=1, then this will generate numbers in the range
-- [-8.57,8.57] assuing that the uniform RNG is really giving full
-- precision for doubles.

normal_bm :: (Double,Double) -- ^ (mu,sigma)
	  -> [Double]        -- ^ U
	  -> [Double]        -- ^ X

normal_bm (mu,sigma) u = map (adjust (mu,sigma)) $ normal' u
    where normal' (u1:u2:us) | w <= 1    = x : y : normal' us
			     | otherwise = normal' us
	      where v1 = 2 * u1 - 1
		    v2 = 2 * u2 - 1
		    w  = v1 * v1 + v2 * v2
		    x  = v1 * sqrt (-2 * log w / w)
		    y  = v2 * sqrt (-2 * log w / w)

-- | Acceptance-Rejection Method (Ross, pp 448--450)
-- 
-- If mu=0 and sigma=1, then this will generate numbers in the range
-- [-36.74,36.74] assuming that the uniform RNG is really giving full
-- precision for doubles.

normal_ar :: (Double,Double) -- ^ (mu,sigma)
	  -> [Double]        -- ^ U
	  -> [Double]        -- ^ X

normal_ar (mu,sigma) u = map (adjust (mu,sigma)) $ normal' u
    where normal' (u1:u2:u3:us) | y > 0     = z : normal' us
				| otherwise = normal' (u3:us)
	      where y1 = -log u1
		    y2 = -log u2
		    y  = y2 - (y1 - 1)^2 / 2
		    z | u3 <= 0.5 =  y1
		      | u3 >  0.5 = -y1

-- | Ratio Method (Kinderman-Monahan) (Knuth, v2, 2ed, pp 125--127)
-- 
-- If mu=0 and sigma=1, then this will generate numbers in the range
-- [-1e15,1e15] (?) assuming that the uniform RNG is really giving full
-- precision for doubles.

normal_r :: (Double,Double) -- ^ (mu,sigma)
	 -> [Double]        -- ^ U
	 -> [Double]        -- ^ X

normal_r (mu,sigma) u = map (adjust (mu,sigma)) $ normal' u
    where normal' (u:v:us) | x^2 <= -4 * log u = x : normal' us
			   | otherwise         = normal' us
	      where x = a * (v - 0.5) / u
		    a = 1.71552776992141359295 -- sqrt $ 8 / e
