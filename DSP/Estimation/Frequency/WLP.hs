-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Estimation.Frequency.WLP
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- This module contains a few algorithms for weighted linear predictors
-- for estimating the frequency of a complex sinusoid in noise.
--
-----------------------------------------------------------------------------

-- Boy, fromIntegral makes these look really messy.

module DSP.Estimation.Frequency.WLP where

import Data.Array
import Data.Complex

-- | The weighted linear predictor form of the frequency estimator

wlp :: (Ix a, Integral a, RealFloat b) => Array a b -- ^ window
    -> Array a (Complex b) -- ^ z
    -> b -- ^ w

wlp w z = phase (sum [ (w!t :+ 0) * z!t * conjugate (z!(t-1)) | t <- [1..(n-1)] ])
    where n = snd (bounds z) + 1

-- | WLP using Lank, Reed, and Pollon's window

lrp :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ z
    -> b -- ^ w

lrp z = wlp (array (1,n-1) [ (t, 1 / fromIntegral (n-1)) | t <- [1..(n-1)] ]) z
    where n = snd (bounds z) + 1

-- | WLP using kay's window

kay :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ z
    -> b -- ^ w

kay z = wlp (array (1,n-1) [ (t, fromIntegral (6*t*(n-t)) / fromIntegral (n*(n^2-1))) | t <- [1..(n-1)] ]) z
    where n = snd (bounds z) + 1

-- | WLP using Lovell and Williamson's window

lw :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ z
    -> b -- ^ w

lw z = wlp (array (1,n-1) [ (t, fromIntegral (6*t*(n-t)) / (fromIntegral (n*(n^2-1)) * magnitude (z!t) * magnitude (conjugate (z!(t-1))))) | t <- [1..(n-1)] ]) z
    where n = snd (bounds z) + 1

-- | WLP using Clarkson, Kootsookos, and Quinn's window

ckq :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ z
    -> b -- ^ rho
    -> b -- ^ sigma
    -> b -- ^ w

ckq z rho sig = wlp (array (1,n-1) [ (t, num t / den) | t <- [1..(n-1)] ]) z
    where num t = sinh (fromIntegral n * th) - sinh (fromIntegral t * th) - sinh (fromIntegral (n-t) * th)
	  den = fromIntegral (n-1) * sinh (fromIntegral n * th) - 2 * sinh (0.5 * fromIntegral n * th) * sinh (0.5 * fromIntegral (n-1) * th) / sinh (0.5 * th)
	  th = log (1 + sig^2 / rho^2 + sqrt (sig^4 / rho^4 + sig^2 / rho^2))
	  n = snd (bounds z) + 1
