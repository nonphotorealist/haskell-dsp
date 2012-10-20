-- Simple demo that demonstrates colored Gaussian noise

module Main (main) where

-- Import the System functions that we need

import System.Environment
import System.IO
import System.Exit

-- We need support for complex numbers and arrays

import Data.Complex
import Data.Array

-- Import a portion of the Numeric.Random library

import Numeric.Random.Generator.MT19937
import Numeric.Random.Distribution.Uniform
import Numeric.Random.Distribution.Normal
import Numeric.Random.Spectrum.White
import Numeric.Random.Spectrum.Pink
import Numeric.Random.Spectrum.Purple
import Numeric.Random.Spectrum.Brown

-- We do some simple FFT analysis

import Numeric.Transform.Fourier.FFT

-- Noise parameters

mu :: Double
mu = 0

sigma :: Double
sigma = 1

-- u is our list of uniforms over (0,1]

u :: [Double]
u = uniform53oc $ genrand 42

-- x is our list of normal random variables

x :: [Double]
x = normal_ar (mu,sigma) u

-- white: flat power spectrum

white_gn :: [Double]
white_gn = white $ x

-- pink: -3 dB/octave or -10 dB/decade

pink_gn :: [Double]
pink_gn = kellet $ white_gn

-- brown: -6 dB/octave or -20 dB/decade

brown_gn :: [Double]
brown_gn = brown $ white_gn

-- purple: +6 dB/octave or +20 dB/decade

purple_gn :: [Double]
purple_gn = purple $ white_gn

-- dbrfft caluclates the magnitude response of the input, and subtracts
-- out the power of the integration window

dbrfft :: Array Int Double -> Array Int Double
dbrfft xs = fmap db $ rfft $ xs
    where db (r:+i) = 10 * log10 (r*r+i*i) - 10 * log10 n
	  log10 = logBase 10
	  n = fromIntegral $ snd (bounds xs) + 1

-- avg averages a list of arrays pointwise

avg :: [Array Int Double] -> Array Int Double
avg xs = fmap (/ n) xs'
    where xs' = foldl1 add xs
	  add as bs = listArray (bounds as) $ zipWith (+) (elems as) (elems bs)
          n = fromIntegral $ length xs

-- chunk creates n3 sublists from xs of n1 elemets, and overlapping 
-- n2 points

chunk :: Int -> Int -> Int -> [Double] -> [[Double]]
chunk n1 n2 n3 xs = take n1 xs : chunk n1 n2 n3 (drop (n1-n2) xs)

-- avg calculates an averaged RFFT using a rectangular window
--   n1 is the length of each FFT
--   n2 is the overlap
--   n3 is the number of FFTs to average

avgrfft :: Int -> Int -> Int -> [Double] -> Array Int Double
avgrfft n1 n2 n3 xs = avg $ take n3 $ map dbrfft $ map (listArray (0,n1-1)) $ chunk n1 n2 n3 xs

-- simple function to write out an array to a file

dump :: String -> Array Int Double -> IO ()
dump filename xs = do h <- openFile filename WriteMode
		      sequence $ map (dump' h) $ assocs $ xs
		      hClose h
    where dump' h (f,m) = do hPutStr h   $ show f
			     hPutStr h   $ " "
			     hPutStrLn h $ show m

-- usage function

usage :: IO a
usage = do self <- getProgName
	   putStrLn $ "usage: " ++ self ++ " n1 n2 n3"
	   putStrLn $ "       where n1 = FFT length"
	   putStrLn $ "             n2 = overlap"
	   putStrLn $ "             n3 = number of FFTs to average"
           exitFailure

-- simple function to parse the command line

parseargs :: IO (Int,Int,Int)
parseargs = do args <- getArgs
	       if length args == 3
		  then do let n1 = read $ args !! 0
			      n2 = read $ args !! 1
			      n3 = read $ args !! 2
			  return (n1,n2,n3)
		  else usage

-- glue it all together

main :: IO ()
main = do (n1,n2,n3) <- parseargs
	  dump "white.out"  $ avgrfft n1 n2 n3 $ white_gn
	  dump "pink.out"   $ avgrfft n1 n2 n3 $ pink_gn
	  dump "brown.out"  $ avgrfft n1 n2 n3 $ brown_gn
	  dump "purple.out" $ avgrfft n1 n2 n3 $ purple_gn
	  return ()
