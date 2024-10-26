{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
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
module Main where

import Data.Time
import Data.Machine
import System.Info
import System.Environment(getArgs)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- | Decimal time types
newtype Seconds = Seconds Double
  deriving (Eq, Ord, Num, Fractional, Real, RealFrac)

newtype Days = Days Double
  deriving (Eq, Ord, Num, Fractional, Real, RealFrac)

newtype DecimalTime = DecimalTime Integer
  deriving (Eq, Show, Ord, Num, Enum, Real, Integral)

-- | Validation for our decimal time values
newtype ValidDecimalTime = ValidDecimalTime
  {unVDT :: DecimalTime}
  deriving (Show, Eq)

-- | Pure functions used to calculate the decimal time from Data.Time.getZonedTime
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

-- | Validate the DecimalTime calculated
{-# INLINEABLE mkVDT #-}
mkVDT :: DecimalTime -> Either String ValidDecimalTime
mkVDT dt@(DecimalTime t)
  | t >= 0 && t <= 1000 = Right $ ValidDecimalTime dt
  | otherwise = Left "Time must be between 0 and 1000"

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

-- | Retrieve initial time and create our process (producer)
zone :: ProcessT IO k ZonedTime
zone = construct $ do
  zt <- liftIO getZonedTime
  yield zt

-- | Format the output of the validation
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
      DecimalTime 0 -> "NEW"
      DecimalTime t -> T.pack . show $ t

-- | Output the valid decimal time (consumer)
result :: ProcessT IO T.Text ()
result = construct $ await >>= liftIO . TIO.putStrLn

-- | Get platform information for version string
platform :: T.Text
platform = "(" <> T.pack arch <> "-" <> T.pack os <> ")"

-- | Show version information if the user types -v
version :: IO ()
version =
  TIO.putStrLn $
    "Decimal time clock that maps your day to 1000 decimal minutes, "
      <> "version 1.0.0 "
      <> platform

-- | If any other command line argument other than -v is given we show the only valid command is -v
validArgs :: IO ()
validArgs = TIO.putStrLn "Valid arguments are: -v, --version"

-- | Process args or continue with running the machine
{-# INLINE runD #-}
runD :: [String] -> IO ()
runD = \case
  ["-v"] -> version
  ["--version"] -> version
  [] -> runClock
  _ -> validArgs
  where
    runClock =
      runT_ $
        zone
          ~> mapping loc
          ~> mapping dec
          ~> mapping fmt
          ~> result
    {-# INLINE runClock #-}

main :: IO ()
main = getArgs >>= runD
