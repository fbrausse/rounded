{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.MPFR.LowLevel
-- Copyright   :  (C) 2012-2014 Edward Kmett, Daniel Peebles
--                (C) 2013-2017 Claude Heiland-Allen
--                (C) 2017 Michal Konecny
-- License     :  LGPL
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Numeric.MPFR.LowLevel
(
-- * Floating point numbers with a specified rounding mode and precision
  MPFR
-- , fromInt
-- , fromDouble
, set
, toString
-- * operations analogous to RealFloat
, onMPFR_floatRadix
, onMPFR_floatDigits
, onMPFR_decodeFloat
, onMPFR_encodeFloat
, onMPFR_isNaN
, onMPFR_isInfinite
, onMPFR_isDenormalized
, onMPFR_isNegativeZero
-- * Precision and Rounding
, module Numeric.MPFR.RoundPrec
-- * Conversions
, toDouble
, toRational'
, fromRational'
, toInteger'
, fromInteger'
, onMPFR_properFraction
, onMPFR_round
-- * Useful Constants
, kPi
, kLog2
, kEuler
, kCatalan

-- * Combinators that are oblivious to precision
-- , succUlp
-- , predUlp
-- * Mixed-precision operations
, add_
, sub_
, mul_
, div_
, abs_
, negate_
, compare_
, min_
, max_
, modf
, sqrt_
, exp_
, log_
, sin_
, tan_
, cos_
, asin_
, atan_
, atan2_
, acos_
, sinh_
, tanh_
, cosh_
, asinh_
, atanh_
, acosh_
, log1p_
, expm1_
)
where

-- import Control.Exception (bracket, bracket_, throwIO, ArithException(Overflow))
import Control.Exception (bracket, throwIO, ArithException(Overflow))
import Data.Bits (shiftL, testBit)
-- import Data.Int (Int32)
import Data.Ratio ((%))

-- import Foreign (with, alloca, allocaBytes, peek, sizeOf, nullPtr)
import Foreign (with, allocaBytes, peek, sizeOf, nullPtr)
import Foreign.C (CInt(..), CIntMax(..)) --, CSize(..), CChar(..))
import Foreign.C.String (peekCString)

import System.IO.Unsafe (unsafePerformIO)

-- import GHC.Prim
--   ( ByteArray#
--   , sizeofByteArray#
--   , copyByteArrayToAddr#
--   , newByteArray#
--   , copyAddrToByteArray#
--   , unsafeFreezeByteArray#
--   )
-- import GHC.Types (IO(..))
import GHC.Exts (Ptr) -- (..), Int(..))

#if MIN_VERSION_base(4,9,0)
import Numeric (Floating(..))
#endif
-- import Numeric (readSigned, readFloat)

import Numeric.GMP.Utils (withInInteger, withOutInteger, withOutInteger_, withInRational)
import Numeric.GMP.Types (MPLimb)

import Numeric.MPFR.Types
import Numeric.MPFR.RoundPrec
import Numeric.MPFR.Functions

cmp' :: Comparison -> MPFR -> MPFR -> CInt
cmp' f a b = unsafePerformIO $
  in_ a $ \afr ->
  in_ b $ \bfr -> do
  f afr bfr

cmp :: Comparison -> MPFR -> MPFR -> Bool
cmp f a b = cmp' f a b /= 0

compare_ :: MPFR -> MPFR -> Ordering
compare_ a b = compare (cmp' mpfr_cmp a b) 0

instance Eq MPFR where
  (==) = cmp mpfr_equal_p
  (/=) = cmp mpfr_lessgreater_p

instance Ord MPFR where
  compare a b = compare (cmp' mpfr_cmp a b) 0
  (<=) = cmp mpfr_lessequal_p
  (>=) = cmp mpfr_greaterequal_p
  (<) = cmp mpfr_less_p
  (>) = cmp mpfr_greater_p
  min = binaryExact mpfr_min
  max = binaryExact mpfr_max

-- | Round to 'Double' with the given rounding mode.
toDouble :: RoundingMode -> MPFR -> Double
toDouble r x = unsafePerformIO $ in_ x $ \xfr -> mpfr_get_d xfr (rnd r)
{-# RULES "realToFrac/toDouble" forall (x :: MPFR) . realToFrac x = toDouble TowardNearest x #-}

{-| Returns the value of op with given Precision and rounded with RoundingMode. -}
set :: RoundingMode -> Precision -> MPFR -> MPFR
set = unary mpfr_set

sgn :: MPFR -> Ordering
sgn x = compare (unsafePerformIO $ in_ x mpfr_sgn) 0

tst :: Test -> MPFR -> Bool
tst f x = unsafePerformIO $ in_ x $ \xfr -> do
  t <- f xfr
  return (t /= 0)

decodeFloat' :: MPFR -> (Integer, Int)
decodeFloat' x = case (unsafePerformIO $ do
  in_ x $ \xfr -> withOutInteger $ \xz -> with 0 $ \flagsptr -> do
    e <- wrapped_mpfr_get_z_2exp xz xfr flagsptr
    flags <- peek flagsptr
    case testBit flags erangeBit of
      False -> return (fromIntegral e)
      True -> throwIO Overflow) of
  (0, _) -> (0, 0) -- mpfr_get_z_2exp returns emin instead of 0 for exponent
  me -> me

erangeBit :: Int
erangeBit = 5 -- sync with cbits/wrappers.c

encodeFloat' :: Precision -> Integer -> Int -> MPFR
encodeFloat' p j e = r where
  r = unsafePerformIO $ do
        (Just x, _) <- withInInteger j $ \jz -> out_ p $ \xfr -> mpfr_set_z_2exp xfr jz (fromIntegral e) (rnd TowardNearest)
        return x

-- functions analogous to methods of RealFloat:
onMPFR_floatRadix :: MPFR -> Integer
onMPFR_floatRadix  _ = 2
onMPFR_floatDigits :: MPFR -> Int
onMPFR_floatDigits = getPrec

--   -- FIXME: this should do for now, but the real ones can change...
-- mpfr_floatRange _ = (fromIntegral (minBound :: Int32), fromIntegral (maxBound :: Int32))

onMPFR_decodeFloat :: MPFR -> (Integer, Int)
onMPFR_decodeFloat = decodeFloat'
onMPFR_encodeFloat :: Precision -> Integer -> Int -> MPFR
onMPFR_encodeFloat = encodeFloat'
onMPFR_isNaN :: MPFR -> Bool
onMPFR_isNaN = tst mpfr_nan_p
onMPFR_isInfinite :: MPFR -> Bool
onMPFR_isInfinite = tst mpfr_inf_p
onMPFR_isDenormalized :: MPFR -> Bool
onMPFR_isDenormalized _ = False
onMPFR_isNegativeZero :: MPFR -> Bool
onMPFR_isNegativeZero r = tst mpfr_zero_p r && tst mpfr_signbit r
-- mpfr_isIEEE _ = True -- is this a lie? it mostly behaves like an IEEE float, despite being much bigger
-- mpfr_atan2 = atan2_

instance Show MPFR where
  show x = toString 30 x

toString :: Int -> MPFR -> String
-- FIXME: what do about unsightly 0.1 -> 0.1000...0002 or 9.999...9995e-2 issues
toString maxN x = unsafePerformIO $ do
  (s, e) <- in_ x $ \xfr -> with 0 $ \eptr -> do
    s <- bracket (mpfr_get_str nullPtr eptr 10 0 xfr (rnd TowardNearest)) mpfr_free_str peekCString
    e <- peek eptr
    return (s, fromIntegral e)
  return $ case () of
    _ | onMPFR_isNaN x -> "NaN"
      | onMPFR_isInfinite x && sgn' == GT -> "Infinity"
      | onMPFR_isInfinite x -> "-Infinity"
      | onMPFR_isNegativeZero x -> "-0.0"
      | sgn' == EQ -> "0.0"
      | e <  0 ||
        e >= threshold -> sign ++ take 1 digits  ++ "." ++
                          dropTrailingZeroes (take (n - 1) (drop 1 digits0)) ++
                          "e" ++ show (e - 1)
      | e == 0         -> sign ++ "0." ++
                          dropTrailingZeroes digits
      | e <  threshold -> sign ++ take e digits0 ++ "." ++
                          dropTrailingZeroes (take (n - e) (drop e digits0))
      | otherwise -> error "internal error in toString"
      where
        sgn' = sgn x
        sign = case sgn' of
          GT -> ""
          EQ -> ""
          LT -> "-"
        threshold = 8
        n = min maxN $ length digits
        digits = case take 1 s of
          "-" -> drop 1 s
          _ -> s
        digits0 = digits ++ repeat '0'
        dropTrailingZeroes a = case dropWhile ('0' ==) (reverse a) of
          "" -> "0"
          b -> reverse b




kPi, kLog2, kEuler, kCatalan :: RoundingMode -> Precision -> MPFR
kPi = constant mpfr_const_pi
kLog2 = constant mpfr_const_log2
kEuler = constant mpfr_const_euler
kCatalan = constant mpfr_const_catalan

abs_, negate_, log_, exp_, sqrt_,
 sin_, cos_, tan_, asin_, acos_, atan_,
   sinh_, cosh_, tanh_, asinh_, acosh_, atanh_,
     log1p_, expm1_
  :: RoundingMode -> Precision -> MPFR -> MPFR
abs_ = unary mpfr_abs
negate_ = unary mpfr_neg
log_ = unary mpfr_log
exp_ = unary mpfr_exp
sqrt_ = unary mpfr_sqrt
sin_ = unary mpfr_sin
cos_ = unary mpfr_cos
tan_ = unary mpfr_tan
asin_ = unary mpfr_asin
acos_ = unary mpfr_acos
atan_ = unary mpfr_atan
sinh_ = unary mpfr_sinh
cosh_ = unary mpfr_cosh
tanh_ = unary mpfr_tanh
asinh_ = unary mpfr_asinh
acosh_ = unary mpfr_acosh
atanh_ = unary mpfr_atanh
log1p_ = unary mpfr_log1p
expm1_ = unary mpfr_expm1

min_, max_, add_, sub_, mul_, div_, atan2_
  :: RoundingMode -> Precision -> MPFR -> MPFR -> MPFR
min_ = binary mpfr_min
max_ = binary mpfr_max
add_ = binary mpfr_add
sub_ = binary mpfr_sub
mul_ = binary mpfr_mul
div_ = binary mpfr_div
atan2_ = binary mpfr_atan2

toRational' :: MPFR -> Rational
toRational' r
   | e > 0     = fromIntegral (s `shiftL` e)
   | otherwise = s % (1 `shiftL` negate e)
   where (s, e) = decodeFloat' r

fromRational' :: RoundingMode -> Precision -> Rational -> MPFR
fromRational' r p q = result where -- TODO small integer optimisation
  result = unsafePerformIO $ do
        (Just x, _) <- withInRational q $ \qq -> out_ p $ \qfr -> mpfr_set_q qfr qq (rnd r)
        return x

modf :: RoundingMode -> Precision -> MPFR -> (MPFR, MPFR)
modf r p x = unsafePerformIO $ do
  (Just y, (Just z, _)) <- in_ x $ \xfr ->
    out_ p $ \yfr ->
      out_ p $ \zfr ->
        mpfr_modf yfr zfr xfr (rnd r)
  return (y, z)

toInteger' :: RoundingMode -> MPFR -> Integer
toInteger' r x = unsafePerformIO $
  withOutInteger_ $ \yz ->
    in_ x $ \xfr ->
      with 0 $ \flagsptr -> do
        e <- wrapped_mpfr_get_z yz xfr (rnd r) flagsptr
        flags <- peek flagsptr
        case testBit flags erangeBit of
          False -> return e
          True -> throwIO Overflow

fromInteger' :: RoundingMode -> Precision -> Integer -> MPFR
fromInteger' r p j = result where
  result = unsafePerformIO $ do
        if toInteger (minBound :: CIntMax) <= j && j <= toInteger (maxBound :: CIntMax)
        then do
          (Just x, _) <- out_ p $ \jfr -> mpfr_set_sj jfr (fromInteger j :: CIntMax) (rnd r)
          return x
        else do
          (Just x, _) <- withInInteger j $ \jz -> out_ p $ \jfr -> mpfr_set_z jfr jz (rnd r)
          return x

onMPFR_properFraction :: (Integral a) => RoundingMode -> Precision -> MPFR -> (a, MPFR)
onMPFR_properFraction r p x = (fromInteger (toInteger' r i), f) where
  (i, f) = modf r p x
-- this round is from base-4.9.1.0, modified to use compare instead of signum
onMPFR_round :: (Integral a) => RoundingMode -> Precision -> MPFR -> a
onMPFR_round r p x =
          let (n,f) = onMPFR_properFraction r p x
              m     = if tst mpfr_signbit f then n - 1 else n + 1
          in  case compare_ (abs_ r p x) (fromRational' Up 10 0.5) of
                LT -> n
                EQ -> if even n then n else m
                GT -> m

------------------------------------------------------------

constant :: Constant -> RoundingMode -> Precision -> MPFR
constant k r p = result where
  result = unsafePerformIO $ do
    (Just x, _) <- out_ p $ \xfr -> k xfr (rnd r)
    return x

unary :: Unary -> RoundingMode -> Precision -> MPFR -> MPFR
unary f r p a = unsafePerformIO $ do
  (Just c, _) <- in_ a $ \afr ->
    out_ p $ \cfr ->
      f cfr afr (rnd r)
  return c

binary :: Binary -> RoundingMode -> Precision -> MPFR -> MPFR -> MPFR
binary f r p a b = unsafePerformIO $ do
  (Just c, _) <-
    in_ a $ \afr ->
      in_ b $ \bfr ->
        out_ p $ \cfr ->
          f cfr afr bfr (rnd r)
  return c

binaryExact :: Binary -> MPFR -> MPFR -> MPFR
binaryExact f x y = binary f TowardNearest p x y
  where
  p = getPrec x `max` getPrec y

in_ :: MPFR -> (Ptr MPFR -> IO a) -> IO a
in_ x f = with x f

out_ :: Precision -> (Ptr MPFR -> IO a) -> IO (Maybe MPFR, a)
out_ pP f =
  let p = prec pP in
  allocaBytes (precBytes p) $ \d -> with
  MPFR{ mpfrPrec = p, mpfrSign = 0, mpfrExp = 0, mpfrD = d } $ \ptr -> do
    a <- f ptr
    result@MPFR{ mpfrPrec = p', mpfrSign = _, mpfrExp = _, mpfrD = _ } <- peek ptr
    if p /= p' then return (Nothing, a) else return (Just result, a)

-- "The number of limbs in use is controlled by _mpfr_prec, namely ceil(_mpfr_prec/mp_bits_per_limb)."
-- <http://www.mpfr.org/mpfr-current/mpfr.html#Internals>
precBytes :: MPFRPrec -> Int
precBytes p = bytesPerLimb * ((fromIntegral p + bitsPerLimb1) `div` bitsPerLimb)
bytesPerLimb :: Int
bytesPerLimb = sizeOf (undefined :: MPLimb)
bitsPerLimb :: Int
bitsPerLimb = bytesPerLimb * 8
bitsPerLimb1 :: Int
bitsPerLimb1 = bitsPerLimb - 1
