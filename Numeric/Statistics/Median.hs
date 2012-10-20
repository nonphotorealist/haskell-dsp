-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Statistics.Median
-- Copyright   :  (c) Matthew Donadio 2002
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Simple module for computing the median on a list
--
-- Reference: Ross, NRiC
--
-----------------------------------------------------------------------------

module Numeric.Statistics.Median (median) where

import Data.List

-- | Compute the median of a list

median :: (Ord a, Fractional a) => [a] -> a
median x | odd n  = sort x !! (n `div` 2)
         | even n = ((sort x !! (n `div` 2 - 1)) + (sort x !! (n `div` 2))) / 2
    where n = length x
