{-# LANGUAGE BangPatterns #-}
module Curly.Core.Security.EC(EC(..),basePoint,baseCurve,padd,pmul) where

import Definitive
import qualified Data.Bits as B
import qualified Data.List as L
import qualified Numeric as N
import qualified Data.Char as DC

data EC = EC {
  getl :: Int,
  getp,getr,geta,getb :: Integer
  }

type ECP = (Integer,Integer)

baseCurve :: EC
baseCurve = p256
  where p256 = EC l p r a b 
          where l = 256 ::  Int
                p = 115792089210356248762697446949407573530086143415290314195533631308867097853951
                r = 115792089210356248762697446949407573529996955224135760342422259061068512044369
                a = 115792089210356248762697446949407573530086143415290314195533631308867097853948
                b = 41058363725152142129326129780047268409114441015993725554835256314039467401291
basePoint :: ECP
basePoint = p256
  where p256 = (x,y)
          where x = 48439561293906451759052585252797914202762949526041747995844080717082404635286
                y = 36134250956749795798585127919587881956611106672985015071877198253568414405109

-- |extended euclidean algorithm, recursive variant
eeukl :: (Integral a,Disjonctive a,Semiring a) => a -> a -> (a, a, a)
eeukl a 0 = (a,1,0)
eeukl a b = let (d,s,t) = eeukl b (a `mod` b)
            in (d,t,s-(div a b)*t)

-- |computing the modular inverse of @a@ `mod` @m@
modinv :: (Integral a,Disjonctive a,Semiring a) => a -- ^the number to invert
       -> a -- ^the modulus
       -> a -- ^the inverted value
modinv a m = let (x,y,_) = eeukl a m
             in if x == 1 
                then mod y m
                else undefined

pdouble :: ECP -> ECP
pdouble (x1,y1) = 
  let alpha = geta baseCurve
      p = getp baseCurve
      lambda = ((3*x1^(2::Int)+alpha)*(modinv (2*y1) p)) `mod` p
      x3 = (lambda^(2::Int) - 2*x1) `mod` p
      y3 = (lambda*(x1-x3)-y1) `mod` p
  in (x3,y3)

padd :: ECP -> ECP -> ECP
padd a@(x1,y1) b@(x2,y2) 
  | x1==x2,y1==(-y2) = (x1,y2)
  | a==b = pdouble a
  | otherwise = 
      let lambda = ((y2-y1)*(modinv (x2-x1) p)) `mod` p
          x3 = (lambda^(2::Int) - x1 - x2) `mod` p
          y3 = (lambda*(x1-x3)-y1) `mod` p
      in (x3,y3)
  where p = getp baseCurve

pmul :: ECP -> Integer -> ECP
pmul b k' = 
  let p = getp baseCurve
      k = k' `mod` (p - 1)
      ex !p1 !p2 i
        | i < 0 = p1
        | not (B.testBit k i) = ex (pdouble p1) (padd p1 p2) (i - 1)
        | otherwise = ex (padd p1 p2) (pdouble p2) (i - 1)
  in ex b (pdouble b) ((L.length (binary k)) - 2)

-- |binary representation of an integer
-- |taken from http://haskell.org/haskellwiki/Fibonacci_primes_in_parallel
binary :: Integer -> String
binary = flip (N.showIntAtBase 2 DC.intToDigit) []


  
