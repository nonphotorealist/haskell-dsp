-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Random.Generator.MT19937
-- Copyright   :  (c) Matt Harden 1999
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- A Haskell program for MT19937 pseudorandom number generator
--
-----------------------------------------------------------------------------

-- The original source was found at
--
-- http://members.primary.net/~matth/mt19937.hs
--
-- but I can't get to the site anymore.  As much as the orginal
-- formatting has been retained as possible. --mpd


{-
   Function genrand generates an infinite list of pseudorandom 
   unsigned integers (32bit) which are uniformly distributed
   among 0 to 2^32-1.  sgenrand(seed) uses an algorithm of Knuth
   to provide 624 initial values to genrand(). 

   Rewritten in Haskell by Matt Harden
      from original code in C by Takuji Nishimura.

   This program relies upon the GHC/Hugs extensions to Haskell.
   These are very likely to be available in any Haskell
   environment, and performance would suffer greatly without them.
-}

{-
   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later
   version.
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
   See the GNU Library General Public License for more details.
   You should have received a copy of the GNU Library General
   Public License along with this library; if not, write to the
   Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307  USA
-}

-- Copyright (C) 1999 Matt Harden
-- The original C code contained the following notice:
--   When you use this, send an email to: matumoto@math.keio.ac.jp
--   with an appropriate reference to your work.

{- REFERENCE -
   M. Matsumoto and T. Nishimura,
   "Mersenne Twister: A 623-Dimensionally Equidistributed Uniform
   Pseudo-Random Number Generator",
   ACM Transactions on Modeling and Computer Simulation,
   Vol. 8, No. 1, January 1998, pp 3--30.
-}

module Numeric.Random.Generator.MT19937 (W, genrand) where

import Data.Word
import Data.Bits

infixl 8 .<<., .>>.

(.<<.), (.>>.) :: (Bits a) => (a -> Int -> a)
(.<<.) = shiftL
(.>>.) = shiftR

type W = Word32

-- Period parameters
parm_N = 624 :: Int
parm_M = 397 :: Int
parm_A = 0x9908b0df :: W
uPPER_MASK = (bit 31) :: W
lOWER_MASK = (complement uPPER_MASK) :: W

-- Tempering parameters
tEMPERING_MASK_B = (.&. 0x9d2c5680) :: W -> W
tEMPERING_MASK_C = (.&. 0xefc60000) :: W -> W
tEMPERING_SHIFT_U = (.>>. 11) :: W -> W
tEMPERING_SHIFT_S = (.<<.  7) :: W -> W
tEMPERING_SHIFT_T = (.<<. 15) :: W -> W
tEMPERING_SHIFT_L = (.>>. 18) :: W -> W

-- A Knuth algorithm just to seed the seed...
-- Line 25 of table 1
-- in [KNUTH 1981, The Art of Computer Programming Vol. 2 (2nd Ed.), pp102]
sgenrand :: W -> [W]
sgenrand 0 = sgenrand 4357   -- 0 not acceptable.  Why 4357?  I dunno.
sgenrand seed = take parm_N (iterate (69069 *) seed)

mag01 :: W -> W
mag01 0 = 0
mag01 1 = parm_A

tempering :: W -> W
tempering = let (^=) x f = xor x (f x) in
   (^= (tEMPERING_SHIFT_L)) .
   (^= (tEMPERING_MASK_C . tEMPERING_SHIFT_T)) .
   (^= (tEMPERING_MASK_B . tEMPERING_SHIFT_S)) .
   (^= (tEMPERING_SHIFT_U))

-- parameter to rand MUST be a list of (_N) words!
rand :: [W] -> [W]
rand init = map tempering r2 where
   r = init ++ r2
   r2 = zipWith xor (map f r3) (drop parm_M r)
   r3 = zipWith (\x y -> (x .&. uPPER_MASK) .|. (y .&. lOWER_MASK)) r (tail r)
   f y = (y .>>. 1) `xor` (mag01 (y .&. 1))
   
genrand :: W -> [W]
genrand = rand . sgenrand

test = sequence $ map print $ take 1000 $ genrand 4357
