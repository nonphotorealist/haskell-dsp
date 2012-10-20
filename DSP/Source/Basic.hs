-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Source.Basic
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Basic signals
--
-----------------------------------------------------------------------------

module DSP.Source.Basic where

-- | all zeros

zeros :: (Num a) => [a]
zeros = 0 : zeros

-- | single impulse

impulse :: (Num a) => [a]
impulse = 1 : zeros

-- | unit step

step :: (Num a) => [a]
step = 1 : step

-- | ramp

ramp :: (Num a) => [a]
ramp = 0 : zipWith (+) ramp (repeat 1)
