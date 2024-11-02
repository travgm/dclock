{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Travis Montoya 2024
-- License     :  MIT
-- Maintainer  :  trav@hexproof.sh
-- Stability   :  experimental
--
-- Convert time from the systems monotonic clock to decimal time. Viewing the
-- day with usable time left we use 1000 as a number counting down from
-- midnight.
--
-- This is an implementation of the Calculateur concept originally written
-- for the Watchy https://git.sr.ht/~jochen/Calculateur
--
-- Formula:
-- D = |1000 - (1000 * (H * 3600 + M * 60 + S) / 86400)|
-- Where D is decimal time, H is hour, M is minute, S is second
-----------------------------------------------------------------------------
module DecimalTime (
      ClockState(..)
    , updateCurrentDateWithZonedTime
    , localTimeToDecimal
) where

import Types
  ( ClockState (..),
    Days (..),
    DecimalTime (..),
    Seconds (..),
    ValidDecimalTime (..),
    currentDate,
    decimalTime,
    extendedFlag)
import Data.Time
  ( LocalTime (localTimeOfDay),
    TimeOfDay (TimeOfDay),
    ZonedTime (zonedTimeToLocalTime),
    getZonedTime)
import Control.Monad.IO.Class (liftIO)
import Control.Lens ((^.), (&), (?~))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import PrettyPrinter

-- | Pure functions used to calculate the decimal time from Data.Time.getZonedTime
--
-- prop> sec (TimeOfDay 0 0 0) == 0.0 
-- prop> sec (TimeOfDay 1 0 0) == 3600.0 
-- prop> sec (TimeOfDay 24 0 0) == 86400.0
{-# INLINE timeOfDayToSeconds #-}
timeOfDayToSeconds :: TimeOfDay -> Seconds
timeOfDayToSeconds (TimeOfDay !h !m !s) = ((* 3600) h' + (* 60) m') + s'
  where
    h' = fromIntegral h
    m' = fromIntegral m
    s' = realToFrac s

-- | Get fraction of the day
--
-- prop> frac (TimeOfDay 0 0 0) == 0.0
-- prop> frac (TimeOfDay 12 0 0) == 0.5
{-# INLINE timeOfDayToFraction #-}
timeOfDayToFraction :: TimeOfDay -> Days
timeOfDayToFraction = Days . (/ secondsInDay) . (\(Seconds s) -> s) . timeOfDayToSeconds
  where
    secondsInDay = 24 * 60 * 60
    {-# INLINE secondsInDay #-}

-- | Validate the DecimalTime calculated
{-# INLINEABLE mkValidDecimalTime #-}
mkValidDecimalTime :: DecimalTime -> Either String ValidDecimalTime
mkValidDecimalTime dt@(DecimalTime t)
  | t >= 0 && t <= 1000 = Right $ ValidDecimalTime dt
  | otherwise = Left "Time must be between 0 and 1000"

-- | Convert fraction of day to decimal time
--
-- prop> dec (TimeOfDay 0 0 0) == 1000
-- prop> dec (TimeOfDay 12 0 0) == 500
-- prop> dec (TimeOfDay 16 0 0) == 333
{-# INLINE localTimeToDecimal #-}
localTimeToDecimal :: ClockState -> Either String ClockState
localTimeToDecimal s = do
    tod <- readTimeOfDay s                
    vdt <- mkValidDecimalTime (toDecimalMinutes tod)
    return $ s & (decimalTime ?~ vdt)
  where
    {-# INLINE readTimeOfDay #-}
    readTimeOfDay :: ClockState -> Either String TimeOfDay
    readTimeOfDay td = case td ^. currentDate of
        Just lt -> Right $ localTimeOfDay lt
        Nothing -> Left "Current date is not set"

    {-# INLINE toDecimalMinutes #-}
    toDecimalMinutes :: TimeOfDay -> DecimalTime
    toDecimalMinutes = DecimalTime . decimalMinutes . timeOfDayToFraction

    {-# INLINE decimalMinutes #-}
    decimalMinutes :: Days -> Integer
    decimalMinutes = round . (1000 -) . (* 1000)

-- | Transform zoned time to local time
updateCurrentDateWithZonedTime :: ZonedTime -> ClockState -> ClockState
updateCurrentDateWithZonedTime zt state = state & (currentDate ?~ zonedTimeToLocalTime zt)
