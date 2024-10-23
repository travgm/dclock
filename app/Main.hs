{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Travis Montoya 2024
-- License     :  MIT
-- Maintainer  :  trav@hexproof.sh
-- Stability   :  stable
--
-- Convert time from the systems monotonic clock to decimal time. Viewing the
-- day with usable time left we use 1000 as a number counting down from
-- midnight.
--
-- This is an implementation of the Calculateur concept originally written
-- for the Watchy https://git.sr.ht/~jochen/Calculateur
--
-----------------------------------------------------------------------------
module Main where

import Data.Time
import Data.Machine
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- | Type alias's created to help with the abstraction of concrete types to what
-- we are producing with our pure functions and type safety.
newtype Seconds = Seconds Double
    deriving (Eq, Ord, Num, Fractional, Real, RealFrac)

newtype Days = Days Double
    deriving (Eq, Ord, Num, Fractional, Real, RealFrac)

newtype DecimalTime = DecimalTime Int
    deriving (Eq, Show, Ord, Num, Enum, Real, Integral)

-- | Validation for our decimal time values
newtype ValidDecimalTime = ValidDecimalTime 
    { unVDT:: DecimalTime }
    deriving (Show, Eq)

{-# INLINABLE mkVDT #-}
mkVDT :: DecimalTime -> Either String ValidDecimalTime
mkVDT dt@(DecimalTime t)
    | t >= 0 && t <= 1000 = Right $ ValidDecimalTime dt
    | otherwise = Left "Time must be between 0 and 1000"

-- | Pure functions used to calculate the decimal time from Data.Time.getZonedTime
--
-- D = |1000 - (1000 * (H * 3600 + M * 60 + S) / 86400)|
-- Where D is decimal time, H is hour, M is minute, S is second
--
-- prop> sec (TimeOfDay 0 0 0) == 0.0 
-- prop> sec (TimeOfDay 1 0 0) == 3600.0 
-- prop> sec (TimeOfDay 24 0 0) == 86400.0
{-# INLINE sec #-}
sec :: TimeOfDay -> Seconds
sec (TimeOfDay !h !m !s) = ((*3600) h' + (*60) m') + s'
  where
    h' = fromIntegral h
    m' = fromIntegral m
    s' = realToFrac s

-- | Get fraction of the day
--
-- prop> frac (TimeOfDay 0 0 0) == 0.0
-- prop> frac (TimeOfDay 12 0 0) == 0.5
{-# INLINE frac #-}
frac :: TimeOfDay -> Days
frac = Days . (/ secd) . (\(Seconds s) -> s) . sec
  where
    secd = 24 * 60 * 60
    {-# INLINE secd #-}

-- | Convert fraction of day to decimal time
--
-- prop> dec (TimeOfDay 0 0 0) == 1000
-- prop> dec (TimeOfDay 12 0 0) == 500
-- prop> dec (TimeOfDay 16 0 0) == 333
{-# INLINE dec #-}
dec :: TimeOfDay -> Either String ValidDecimalTime
dec = mkVDT . DecimalTime . d
  where
    d = round . (1000 -) . (* 1000) . frac
    {-# INLINE d #-}

-- | Transform zoned time to local time
loc :: ZonedTime -> TimeOfDay
loc = localTimeOfDay . zonedTimeToLocalTime

-- | Retrieve initial time and create our process (producer
zone :: ProcessT IO k ZonedTime
zone = construct $ do
  zt <- liftIO getZonedTime
  yield zt

-- | Format the output of the validation
-- We will either output an error message if decimal time failed validation or we will unwrap
-- and pack our decimal time to be displayed.
--
-- prop> fmt (Right $ ValidDecimalTime (DecimalTime 1000)) == "Decimal time: NEW"
-- prop> fmt (Right $ ValidDecimalTime (DecimalTime 500)) == "Decimal time: 500"
-- prop> fmt (Right $ ValidDecimalTime (DecimalTime 333)) == "Decimal time: 333"
-- prop> fmt (Left "Time must be between 0 and 1000") == "Decimal time: Time must be between 0 and 1000"
{-# INLINE fmt #-}
fmt :: Either String ValidDecimalTime -> T.Text
fmt = ("Decimal time: " <>) . either T.pack (fd . unVDT)
  where
    fd = \case
      DecimalTime 1000 -> "NEW"
      DecimalTime t -> T.pack . show $ t

-- | Output the valid decimal time (consumer)
result :: ProcessT IO T.Text ()
result = construct $ do
  text <- await
  liftIO $ TIO.putStrLn text

-- | The process used below to calculate decimal minutes from the system clock utilizes the machines
-- package to construct a compositional monadic pipeline. A simple way to integrate monadic processing
-- of pure functions with IO.
--
-- Run the machine and transform the data
main :: IO ()
main =
  runT_ $
    zone
      ~> mapping loc
      ~> mapping dec
      ~> mapping fmt
      ~> result
