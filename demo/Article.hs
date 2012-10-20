-- This program was used to generate the data for
--
-- Matthew Donadio, "Lost Knowledge Refound: Sharpened FIR Filters," 
-- IEEE Signal Processing Magazine, to appear

module Main where

import Data.Array

import DSP.Filter.FIR.FIR
import DSP.Filter.FIR.Sharpen
import DSP.Source.Basic

import Numeric.Transform.Fourier.FFTUtils

n :: Int
n = 1000

h :: Array Int Double
h = listArray (0,16) [ -0.016674, -0.022174,  0.015799, 0.047422, -0.013137,
		       -0.090271,  0.021409,  0.31668,  0.48352,   0.31668,
		        0.021409, -0.090271, -0.013137, 0.047422,  0.015799,
		       -0.022174, -0.016674 ]

y1 = fir h         $ impulse
y2 = fir h $ fir h $ impulse
y3 = sharpen h     $ impulse

main = do
       write_rfft_info "y1"  $ listArray (0,999) $ y1
       write_rfft_info "y2"  $ listArray (0,999) $ y2
       write_rfft_info "y3"  $ listArray (0,999) $ y3
