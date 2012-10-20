-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Filter.IIR.Transform
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Digital IIR filter transforms
--
-- Reference: R&G, pg 260; O&S, pg 434; P&M, pg 699
--
-- Notation follows O&S
--
-----------------------------------------------------------------------------

-- TODO: These need more testing.  I checked the lp2hp case against O&S
-- which verifies substitute and lp2hp,nd I triple checked the parameters
-- for the others.  I need to find test vectors for the other cases for
-- proper testing, though.

module DSP.Filter.IIR.Transform (d_lp2lp, d_lp2hp, d_lp2bp, d_lp2bs) where

import Data.Complex

import Polynomial.Basic

normalize :: ([Double],[Double]) -> ([Double],[Double])
normalize (num,den) = (num',den')
    where a0 = last den
	  num' = map (/ a0) num
	  den' = map (/ a0) den

substitute :: ([Double],[Double]) -> ([Double],[Double]) -> ([Double],[Double])
substitute (nsub,dsub) (num,den) = normalize (num',den')
    where n     = max (length num - 1) (length den - 1)
	  num' = step3 $ step2 0 dsub $ step1 n nsub $ num
	  den' = step3 $ step2 0 dsub $ step1 n nsub $ den
          step1 _ _ []     = []
	  step1 n w (x:xs) = map (x*) (polypow w n) : step1 (n-1) w xs
	  step2 _ _ []     = []
          step2 n w (x:xs) = polymult (polypow w n) x : step2 (n+1) w xs
	  step3 x = foldr polyadd [0] x

-- Cotangent

cot :: Double -> Double
cot x = 1 / tan x

-- | Lowpass to lowpass: @z^-1 --> (z^-1 - a)\/(1 - a*z^-1)@

d_lp2lp :: Double -- ^ theta_p
	-> Double -- ^ omega_p
	-> ([Double], [Double]) -- ^ (b,a)
	-> ([Double], [Double]) -- ^ (b',a')

d_lp2lp tp wp (num,den) = substitute (nsub,dsub) (num,den)
    where nsub = [1, -a]
	  dsub = [-a, 1]
	  a = sin ((tp-wp)/2) / sin ((tp+wp)/2)

-- | Lowpass to Highpass: @z^-1 --> -(z^-1 + a)\/(1 + a*z^-1)@

d_lp2hp :: Double -- ^ theta_p
	-> Double -- ^ omega_p
	-> ([Double], [Double]) -- ^ (b,a)
	-> ([Double], [Double]) -- ^ (b',a')

d_lp2hp tp wp (num,den) = substitute (nsub,dsub) (num,den)
    where nsub = [-1, -a]
	  dsub = [a, 1]
	  a = -cos ((tp+wp)/2) / cos ((tp-wp)/2)

-- | Lowpass to Bandpass: z^-1 --> 

d_lp2bp :: Double -- ^ theta_p
	-> Double -- ^ omega_p1
	-> Double -- ^ omega_p2
	-> ([Double], [Double]) -- ^ (b,a)
	-> ([Double], [Double]) -- ^ (b',a')

d_lp2bp tp wp1 wp2 (num,den) = substitute (nsub,dsub) (num,den)
    where nsub = [ 1, -2*a*k/(k+1), (k-1)/(k+1) ]
	  dsub = [ (k-1)/(k+1), -2*a*k/(k+1), 1 ]
	  a = cos ((wp2+wp1)/2) / cos ((wp2-wp1)/2)
	  k = cot ((wp2-wp1)/2) * tan (tp/2)

-- | Lowpass to Bandstop: z^-1 --> 

d_lp2bs :: Double -- ^ theta_p
	-> Double -- ^ omega_p1
	-> Double -- ^ omega_p2
	-> ([Double], [Double]) -- ^ (b,a)
	-> ([Double], [Double]) -- ^ (b',a')

d_lp2bs tp wp1 wp2 (num,den) = substitute (nsub,dsub) (num,den)
    where nsub = [ 1, -2*a/(1+k), (1-k)/(1+k) ]
	  dsub = [ (1-k)/(1+k), -2*a/(1+k), 1 ]
	  a = cos ((wp2+wp1)/2) / cos ((wp2-wp1)/2)
	  k = cot ((wp2-wp1)/2) * tan (tp/2)

{-

Test vectors

O&S, pg 435

 num = polypow  [ 0.001836, 0.001836 ] 4
 den = polymult [ 0.6493, -1.5548, 1 ] [ 0.8482, -1.4996, 1 ]

-}
