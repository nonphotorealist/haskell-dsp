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
import Data.Complex

import Numeric

import Numeric.Random.Generator.MT19937
import Numeric.Random.Distribution.Normal
import Numeric.Random.Distribution.Uniform

import DSP.Source.Oscillator

import Numeric.Transform.Fourier.FFT

import DSP.Estimation.Frequency.Pisarenko
import DSP.Estimation.Frequency.PerMax
import DSP.Estimation.Frequency.FCI
import DSP.Estimation.Frequency.QuinnFernandes
import DSP.Estimation.Frequency.WLP

-- Parameters

rho :: Double
rho = 4.0

w :: Double
w = 0.12345

phi :: Double
phi = 0.23456

snr :: Double
snr = 10

n :: Int
n = 256

-- Vectors

y :: Array Int Double
y = listArray (0,n-1) $ zipWith (+) noise $ map (rho *) $ nco w phi
    where noise = normal_ar (0, sig2) $ uniform53oc $ genrand 42
	  sig2 = (rho^2 / 2) / (10 ** (snr / 10))

z :: Array Int (Complex Double)
z = listArray (0,n-1) $ zipWith (+) noise $ map ((rho :+ 0) *) $ quadrature_nco w phi
    where noise = zipWith (:+) (normal_ar (0, sig2) $ uniform53oc $ genrand 42) (normal_ar (0, sig2) $ uniform53oc $ genrand 43)
          sig2 = (rho^2 / 2) / (10 ** (snr / 10))

-- The tests

dfp z = [ ("Periodigram Maximizer\t\t\t",        permax z k) ]
    where k = round $ w / 2 / pi * fromIntegral n

fci y = [ ("Quinn's First Estimator\t\t\t",       quinn1 y' k / 2),
          ("Quinn's Second Estimator\t\t",        quinn2 y' k / 2),
          ("Quinn's Third Estimator\t\t\t",       quinn3 y' k / 2),
          ("Jacobsen's Third Estimator\t\t",      jacobsen y' k / 2),
          ("MacLeod's Three Point Estimator\t\t", macleod3 y' k / 2),
          ("MacLeod's Five Point Estimator\t\t",  macleod5 y' k / 2),
          ("Rife and Vincent's Estimator\t\t", rv y' k / 2) ]
    where y' = rfft y
          k = round $ w / 2 / pi * fromIntegral n

scm y = [ ("Pisarenko's Method\t\t\t", pisarenko y) ]

offline y = [ ("Quinn-Fernandes\t\t\t\t", qf y w') ]
    where k = round $ w / 2 / pi * fromIntegral n
	  w' = 2 * pi * fromIntegral k / fromIntegral n

fastblock z = [ ("Lank, Reed, and Pollon\t\t\t", lrp z),
		("Kay\t\t\t\t\t", kay z),
		("Lovell and Williamson\t\t\t", lw z) ]
--              ("Clarkson, Kootsookos, and Quinn\t\t", ckq z rho sig) ]
--    where sig = sqrt $ (rho^2 / 2) / (10 ** (snr / 10))

-- Glue it all together

showone (s,w') = putStrLn $ s ++ ": w=" ++ (showFFloat (Just 6) w' $ " err=" ++ showFFloat (Just 6) (abs (w-w')) "")

main = do
       putStrLn "==> Parameters"
       putStrLn $ "rho=\t" ++ show rho
       putStrLn $ "w=\t" ++ show w
       putStrLn $ "phi=\t" ++ show phi
       putStrLn $ "snr=\t" ++ show snr
       putStrLn $ "n=\t" ++ show n
       putStrLn "==> Periodigram Techniques"
       sequence $ map showone $ dfp z
       putStrLn "==> Fourier Coefficient Interpolation Techniques"
       sequence $ map showone $ fci y
       putStrLn "==> Sample Covariance Methods"
       sequence $ map showone $ scm y
       putStrLn "==> Offline Filtering Techniques"
       sequence $ map showone $ offline y
       putStrLn "==> Fast Block Techniques"
       sequence $ map showone $ fastblock z
