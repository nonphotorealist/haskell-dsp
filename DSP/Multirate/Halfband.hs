-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Multirate.Halfband
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Halfband interpolators and decimators
--
-- Reference: C&R
--
-----------------------------------------------------------------------------

module DSP.Multirate.Halfband (hb_interp, hb_decim) where

import Data.Array

import DSP.Basic
import DSP.Filter.FIR.FIR

mkhalfband :: Num a => Array Int a -> Array Int a
mkhalfband h = listArray (0,m `div` 2) [ h!n | n <- [0..m], even n ]
    where m = snd $ bounds h

demux :: Num a => [a] -> ([a],[a])
demux (x:xs) = (demux' (x:xs), demux' xs)
    where demux' []       = []
          demux' (x:[])   = x : []
          demux' (x:_:xs) = x : demux' xs

mux :: Num a => [a] -> [a] -> [a]
mux []     []     = []
mux []     _      = []
mux _      []     = []
mux (x:xs) (y:ys) = x : y : mux xs ys

-- | Halfband interpolator

hb_interp :: (Num a) => Array Int a -- ^ h[n]
	  -> [a] -- ^ x[n]
	  -> [a] -- ^ y[n]

hb_interp h x = mux y1 y2
    where (x1,x2) = demux x
	  y1 = fir (mkhalfband h) x1
	  y2 = map (h!m2 *) $ zn m2 $ x2
	  m2 = (snd $ bounds h) `div` 2

-- | Halfband decimator

hb_decim :: (Num a) => Array Int a -- ^ h[n]
	 -> [a] -- ^ x[n]
	 -> [a] -- ^ y[n]

hb_decim h x = zipWith (+) y1 y2
    where (x1,x2) = demux x
	  y1 = fir (mkhalfband h) x1
	  y2 = map (h!m2 *) $ zn m2 $ x2
	  m2 = (snd $ bounds h) `div` 2
