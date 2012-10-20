-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Transform.Fourier.FFTUtils
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Utility functions based on the FFT
--
-----------------------------------------------------------------------------

module Numeric.Transform.Fourier.FFTUtils (fft_mag, fft_db, fft_phase, fft_grd, fft_info,
			 rfft_mag, rfft_db, rfft_phase, rfft_grd, rfft_info,
	                 write_fft_info, write_rfft_info) where

import System.IO
import Data.Array
import Data.Complex

import Numeric.Transform.Fourier.FFT
import DSP.Unwrap

magsq (x:+y) = x*x + y*y

log10 0 = -1.0e9
log10 x = logBase 10 x

dot a b = realPart a * realPart b + imagPart a * imagPart b

eps = 1.0e-1 :: Double

-- General functions

fft_mag x = fmap magnitude $ fft $ x

fft_db x = fmap (10 *) $ fmap log10 $ fmap magsq $ fft $ x

fft_phase x = unwrap eps $ fmap phase $ fft $ x

fft_grd x = listArray (bounds x') [ dot (x'!i) (dx'!i) / magsq (x'!i) | i <- indices x' ]
    where x'  = fft x
          dx' = fft $ listArray (bounds x) [ fromIntegral i * x!i | i <- indices x ]

fft_info x = (mag,db,arg,grd) 
    where x'  = fft x
          dx' = fft $ listArray (bounds x) [ fromIntegral i * x!i | i <- indices x ]
          mag = fmap magnitude $ x'
	  db  = fmap (10 *) $ fmap log10 $ fmap magsq $ x'
	  arg = unwrap eps $ fmap phase $ x'
	  grd = listArray (bounds x') [ dot (x'!i) (dx'!i) / magsq (x'!i) | i <- indices x' ]

rfft_mag x = fmap magnitude $ rfft $ x

rfft_db x = fmap (10 *) $ fmap log10 $ fmap magsq $ rfft $ x

rfft_phase x = unwrap eps $ fmap phase $ rfft $ x

rfft_grd x = listArray (bounds x') [ dot (x'!i) (dx'!i) / magsq (x'!i) | i <- indices x' ]
    where x'  = rfft x
          dx' = rfft $ listArray (bounds x) [ fromIntegral i * x!i | i <- indices x ]
          dot a b = realPart a * realPart b + imagPart a * imagPart b

-- I/O

rfft_info x = (mag,db,arg,grd) 
    where x'  = rfft x
          dx' = rfft $ listArray (bounds x) [ fromIntegral i * x!i | i <- indices x ]
          mag = fmap magnitude $ x'
	  db  = fmap (10 *) $ fmap log10 $ fmap magsq $ x'
	  arg = unwrap eps $ fmap phase $ x'
	  grd = listArray (bounds x') [ dot (x'!i) (dx'!i) / magsq (x'!i) | i <- indices x' ]

hPrintIndex h n (i,x) = do
                         hPutStr   h $ show (fromIntegral i / fromIntegral n)
			 hPutStr   h $ " "
			 hPutStrLn h $ show x

write_cvector f x = do
	            let n = (snd $ bounds x) + 1
		    h <- openFile f WriteMode
		    sequence $ map (hPrintIndex h n) $ assocs $ x
		    hClose h

write_fft_info b x = do
	             let (mag,db,arg,grd) = fft_info x
		     write_cvector (b ++ "_mag.out") mag
		     write_cvector (b ++ "_db.out")  mag
		     write_cvector (b ++ "_arg.out") mag
		     write_cvector (b ++ "_grd.out") mag

write_rvector f x = do
	            let n = (snd $ bounds x) + 1
		    h <- openFile f WriteMode
		    sequence $ map (hPrintIndex h n) $ take (n `div` 2) $ assocs $ x
		    hClose h

write_rfft_info b x = do
		      let (mag,db,arg,grd) = rfft_info x
		      write_rvector (b ++ "_mag.out") mag
		      write_rvector (b ++ "_db.out")  db
		      write_rvector (b ++ "_arg.out") arg
		      write_rvector (b ++ "_grd.out") grd
