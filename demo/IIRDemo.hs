-- Copyright (c) 2003 Matthew P. Donadio (m.p.donadio@ieee.org)
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

module Main where

import Data.Array

import DSP.Filter.IIR.IIR
import DSP.Filter.IIR.Design

import Numeric.Transform.Fourier.FFTUtils

import DSP.Source.Basic

-- Examples from Oppenheim and Schafer

ex7'3 = mkButterworth (0.2 * pi, 1 - 0.89125) (0.3 * pi, 0.17783)
ex7'8 = mkChebyshev1  (0.2 * pi, 1 - 0.89125) (0.3 * pi, 0.17783)

ex7'5  = mkButterworth (0.4 * pi, 0.01) (0.6 * pi, 0.001)
ex7'6a = mkChebyshev1  (0.4 * pi, 0.01) (0.6 * pi, 0.001)
ex7'6b = mkChebyshev2  (0.4 * pi, 0.01) (0.6 * pi, 0.001)

main = do
       write_rfft_info "ex-7.3"  $ listArray (0,999) $ iir_df1 ex7'3  $ impulse
       write_rfft_info "ex-7.8"  $ listArray (0,999) $ iir_df1 ex7'8  $ impulse
       write_rfft_info "ex-7.5"  $ listArray (0,999) $ iir_df1 ex7'5  $ impulse
       write_rfft_info "ex-7.6a" $ listArray (0,999) $ iir_df1 ex7'6a $ impulse
       write_rfft_info "ex-7.6b" $ listArray (0,999) $ iir_df1 ex7'6b $ impulse
