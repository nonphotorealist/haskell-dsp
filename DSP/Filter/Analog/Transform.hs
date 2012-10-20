-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Filter.Analog.Transform
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Analog prototype filter transforms
--- 
-- Reference: R&G, pg 258; P&M, pg 698
--
-----------------------------------------------------------------------------

module DSP.Filter.Analog.Transform (a_lp2lp, a_lp2hp, a_lp2bp, a_lp2bs) where

import Data.Complex

import Polynomial.Basic

-- Normalizes a filter

normalize (num,den) = (num',den')
    where a0 = last den
	  num' = map (/ a0) num
	  den' = map (/ a0) den

-- | Lowpass to lowpass: @s --> s\/wc@

a_lp2lp :: Double -- ^ wc
	-> ([Double],[Double]) -- ^ (b,a)
	-> ([Double],[Double]) -- ^ (b',a')

a_lp2lp wu (num,den) = normalize (num',den')
    where num' = polysubst [ 0, 1/wu ] num
          den' = polysubst [ 0, 1/wu ] den

-- | Lowpass to highpass: @s --> wc\/s@

a_lp2hp :: Double -- ^ wc
	-> ([Double],[Double]) -- ^ (b,a)
	-> ([Double],[Double]) -- ^ (b',a')

a_lp2hp wu (num,den) = normalize (num',den')
    where nn   = length num
	  nd   = length den
	  n    = max nn nd
	  num' = polysubst [ 0, 1/wu ] $ reverse $ num ++ replicate (n-nn) 0
	  den' = polysubst [ 0, 1/wu ] $ reverse $ den ++ replicate (n-nd) 0

-- | Lowpass to bandpass: @s --> (s^2 + wl*wu) \/ (s(wu-wl))@

a_lp2bp :: Double -- ^ wl
	-> Double -- ^ wu
	-> ([Double],[Double]) -- ^ (b,a)
	-> ([Double],[Double]) -- ^ (b',a')

a_lp2bp wl wu (num,den) = normalize (num',den')
    where n     = max (length num - 1) (length den - 1)
	  num' = step3 $ step2 n [ 0, wu-wl ] $ step1 0 [ wl*wu, 0, 1 ] $ num
          den' = step3 $ step2 n [ 0, wu-wl ] $ step1 0 [ wl*wu, 0, 1 ] $ den
          step1 _ _ []     = []
	  step1 n w (x:xs) = map (x*) (polypow w n) : step1 (n+1) w xs
	  step2 _ _ []     = []
	  step2 n w (x:xs) = polymult (polypow w n) x : step2 (n-1) w xs
	  step3 x = foldr polyadd [0] x

-- | Lowpass to bandstop: @s --> (s(wu-wl)) \/ (s^2 + wl*wu)@

a_lp2bs :: Double -- ^ wl
	-> Double -- ^ wu
	-> ([Double],[Double]) -- ^ (b,a)
	-> ([Double],[Double]) -- ^ (b',a')

a_lp2bs wl wu (num,den) = normalize (num',den')
    where n     = max (length num - 1) (length den - 1)
	  num' = step3 $ step2 n [ wu*wl, 0, 1 ] $ step1 0 [ 0, wu-wl ] $ num
          den' = step3 $ step2 n [ wu*wl, 0, 1 ] $ step1 0 [ 0, wu-wl ] $ den
          step1 _ _ []     = []
	  step1 n w (x:xs) = map (x*) (polypow w n) : step1 (n+1) w xs
	  step2 _ _ []     = []
	  step2 n w (x:xs) = polymult (polypow w n) x : step2 (n-1) w xs
	  step3 x = foldr polyadd [0] x
