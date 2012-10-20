-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Filter.IIR.Design
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Lowpass IIR design functions
--
-- Method:
--
-- (1) Design analog prototype
--
-- 2.  Perform analog-to-analog frequency transformation
--
-- 3.  Perform bilinear transform
--
-----------------------------------------------------------------------------

module DSP.Filter.IIR.Design where

import Data.Array

import DSP.Filter.Analog.Prototype
import DSP.Filter.Analog.Transform
import DSP.Filter.IIR.Bilinear

poly2iir (b,a) = (b',a')
    where b' = listArray (0,m) $ reverse $ b
	  a' = listArray (0,n) $ reverse $ a
          m = length b - 1
	  n = length a - 1

-- | Generates lowpass Butterworth IIR filters

mkButterworth :: (Double, Double) -- ^ (wp,dp)
	      -> (Double, Double) -- ^ (ws,ds)
	      -> (Array Int Double, Array Int Double) -- ^ (b,a)

mkButterworth (wp,dp) (ws,ds) = poly2iir   $ 
			        bilinear 1 $ 
				a_lp2lp wc $ 
				butterworth n
    where n  = ceiling $ log (((1/ds)^2-1) / ((1/(1-dp))^2-1)) / 2 / log (ws' / wp')
	  wc = ws' / ((1/ds)^2-1)**(1/2/fromIntegral n)
	  wp' = prewarp wp 1
	  ws' = prewarp ws 1

-- | Generates lowpass Chebyshev IIR filters

mkChebyshev1 :: (Double, Double) -- ^ (wp,dp)
	     -> (Double, Double) -- ^ (ws,ds)
	     -> (Array Int Double, Array Int Double) -- ^ (b,a)

mkChebyshev1 (wp,dp) (ws,ds) = poly2iir    $ 
			       bilinear 1  $ 
			       a_lp2lp wp' $ 
			       chebyshev1 eps n
    where wp' = prewarp wp 1
          ws' = prewarp ws 1
	  eps = sqrt ((2 - dp)*dp) / (1 - dp)
	  a   = 1 / ds
	  k1  = eps / sqrt (a^2 - 1)
	  k   = wp' / ws'
	  n   = ceiling $ acosh (1/k1) / log ((1 + sqrt (1 - k^2)) / k)

-- | Generates lowpass Inverse Chebyshev IIR filters

mkChebyshev2 :: (Double, Double) -- ^ (wp,dp)
	     -> (Double, Double) -- ^ (ws,ds)
	     -> (Array Int Double, Array Int Double) -- ^ (b,a)

mkChebyshev2 (wp,dp) (ws,ds) = poly2iir    $ 
			       bilinear 1  $ 
			       a_lp2lp ws' $ 
			       chebyshev2 eps n
    where wp' = prewarp wp 1
          ws' = prewarp ws 1
	  eps = ds / sqrt (1 - ds^2)
	  g = 1 - dp
	  n   = ceiling $ acosh (g / eps / sqrt (1 - g^2)) / acosh (ws' / wp')
