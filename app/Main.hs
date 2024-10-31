{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
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
module Main where

import Data.Time
    ( getZonedTime,
      LocalTime(localTimeOfDay),
      TimeOfDay(TimeOfDay),
      ZonedTime(zonedTimeToLocalTime) )
import qualified Data.Time.Format as Time
import Data.Machine as M
import System.Info ( arch, os )
import System.Environment(getArgs)
import Control.Monad.IO.Class (liftIO)
import Control.Lens ((^.), (.~), (&))
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

data ClockState = ClockState
  { _extendedFlag :: Bool,
    _decimalTime  :: ValidDecimalTime,
    _currentDate  :: LocalTime
  }

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens'' s a = Lens s s a a

extendedFlag :: Lens'' ClockState Bool
extendedFlag k (ClockState e d c) = (\e' -> ClockState e' d c) <$> k e

decimalTime :: Lens'' ClockState ValidDecimalTime
decimalTime k (ClockState e d c) = (\d' -> ClockState e d' c) <$> k d

currentDate :: Lens'' ClockState LocalTime
currentDate k (ClockState e d c) = (\c' -> ClockState e d c') <$> k c

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
dec :: ClockState -> Either String ClockState
dec s = case mkDecTime s of
    Right vdt -> Right $ s & decimalTime .~ vdt
    Left err -> Left err
  where
    {-# INLINE rdTod #-}
    rdTod :: ClockState -> TimeOfDay
    rdTod st = localTimeOfDay $ st ^. currentDate

    {-# INLINE toDecMins #-}
    toDecMins :: TimeOfDay -> DecimalTime
    toDecMins = DecimalTime . decTime . frac

    {-# INLINE decTime #-}
    decTime :: Days -> Integer
    decTime = round . (1000 -) . (* 1000)

    {-# INLINE mkDecTime #-}
    mkDecTime :: ClockState -> Either String ValidDecimalTime
    mkDecTime = mkVDT . toDecMins . rdTod

-- | Transform zoned time to local time
loc :: ZonedTime -> ClockState -> ClockState
loc zt state = state & currentDate .~ zonedTimeToLocalTime zt

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
fmt :: Either String ClockState -> T.Text
fmt = \case
    Left err -> "Decimal time: " <> T.pack err
    Right state -> formatTime state
  where
    {-# INLINE formatTime #-}
    formatTime :: ClockState -> T.Text
    formatTime s = "Decimal time: " <> case s ^. decimalTime of
        ValidDecimalTime (DecimalTime 0) -> "NEW" <> extendedInfo s
        ValidDecimalTime (DecimalTime t) -> T.pack (show t) <> extendedInfo s

    {-# INLINE extendedInfo #-}
    extendedInfo :: ClockState -> T.Text
    extendedInfo s = if s ^. extendedFlag
        then " (" <> T.pack (fmtTime $ s ^. currentDate) <> ")"
        else ""

    {-# INLINE fmtTime #-}
    fmtTime :: LocalTime -> String
    fmtTime = Time.formatTime Time.defaultTimeLocale "%Y-%m-%d"

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

-- | If any other command line argument other than -v or --version is given we show help
validArgs :: IO ()
validArgs = TIO.putStrLn "Valid arguments are: -e, -v, --version"

-- | Process args or continue with running the machine
{-# INLINE runD #-}
runD :: [String] -> IO ()
runD = \case
  []            -> runClock False
  ["-e"]        -> runClock True
  ["-v"]        -> version
  ["--version"] -> version
  _             -> validArgs
  where
    runClock e = do
      let i = ClockState e undefined undefined
      runT_ $
        zone
          ~> M.mapping (`loc` i)
          ~> M.mapping dec
          ~> M.mapping fmt
          ~> result

main :: IO ()
main = getArgs >>= runD
