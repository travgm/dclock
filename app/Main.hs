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
import Control.Lens ((^.), (&), (?~))
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
newtype ValidDecimalTime = ValidDecimalTime DecimalTime
  deriving (Show, Eq)

data ClockState = ClockState
  { _extendedFlag :: Bool,
    _decimalTime  :: Maybe ValidDecimalTime,
    _currentDate  :: Maybe LocalTime
  }

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens'' s a = Lens s s a a

extendedFlag :: Lens'' ClockState Bool
extendedFlag k (ClockState e d c) = (\e' -> ClockState e' d c) <$> k e

decimalTime :: Lens'' ClockState (Maybe ValidDecimalTime)
decimalTime k (ClockState e d c) = (\d' -> ClockState e d' c) <$> k d

currentDate :: Lens'' ClockState (Maybe LocalTime)
currentDate k (ClockState e d c) = (\c' -> ClockState e d c') <$> k c

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
localTimeToDecimal s = case mkDecimalTime s of
    Right vdt -> Right $ s & (decimalTime ?~ vdt)
    Left err -> Left err
  where
    {-# INLINE readTimeOfDay #-}
    readTimeOfDay :: ClockState -> TimeOfDay
    readTimeOfDay td = case td ^. currentDate of
        Just lt -> localTimeOfDay lt
        Nothing -> error "Current date is not set"

    {-# INLINE toDecimalMinutes #-}
    toDecimalMinutes :: TimeOfDay -> DecimalTime
    toDecimalMinutes = DecimalTime . decimalMinutes . timeOfDayToFraction

    {-# INLINE decimalMinutes #-}
    decimalMinutes :: Days -> Integer
    decimalMinutes = round . (1000 -) . (* 1000)

    {-# INLINE mkDecimalTime #-}
    mkDecimalTime :: ClockState -> Either String ValidDecimalTime
    mkDecimalTime = mkValidDecimalTime . toDecimalMinutes . readTimeOfDay

-- | Transform zoned time to local time
updateCurrentDateWithZonedTime :: ZonedTime -> ClockState -> ClockState
updateCurrentDateWithZonedTime zt state = state & (currentDate ?~ zonedTimeToLocalTime zt)

-- | Retrieve initial time and create our process (producer)
zonedTime :: ProcessT IO k ZonedTime
zonedTime = construct $ do
  zt <- liftIO getZonedTime
  yield zt

-- | Format the output of the validation
--
-- prop> fmt (Right $ ValidDecimalTime (DecimalTime 1000)) == "Decimal time: NEW"
-- prop> fmt (Right $ ValidDecimalTime (DecimalTime 500)) == "Decimal time: 500"
-- prop> fmt (Right $ ValidDecimalTime (DecimalTime 333)) == "Decimal time: 333"
-- prop> fmt (Left "Time must be between 0 and 1000") == "Decimal time: Time must be between 0 and 1000"
{-# INLINE formatTime #-}
formatTime :: Either String ClockState -> T.Text
formatTime = \case
    Left err -> "Decimal time: " <> T.pack err
    Right state -> createDisplayText state
  where
    {-# INLINE createDisplayText #-}
    createDisplayText :: ClockState -> T.Text
    createDisplayText s =
      "Decimal time: " <> case s ^. decimalTime of
        Just (ValidDecimalTime (DecimalTime 0)) -> "NEW" <> extendedInfo s
        Just (ValidDecimalTime (DecimalTime t)) -> T.pack (show t) <> extendedInfo s
        Nothing -> "Invalid time"

    {-# INLINE extendedInfo #-}
    extendedInfo :: ClockState -> T.Text
    extendedInfo s =
      if s ^. extendedFlag
        then
          ( case s ^. currentDate of
              Just date -> " (" <> T.pack (fmtTime date) <> ")"
              Nothing -> ""
          )
        else ""

    {-# INLINE fmtTime #-}
    fmtTime :: LocalTime -> String
    fmtTime = Time.formatTime Time.defaultTimeLocale "%Y-%m-%d"

-- | Output the valid decimal time (consumer)
displayTimeText :: ProcessT IO T.Text ()
displayTimeText = construct $ await >>= liftIO . TIO.putStrLn

-- | Get platform information for version string
createPlatformText :: T.Text
createPlatformText = "(" <> T.pack arch <> "-" <> T.pack os <> ")"

-- | Show version information if the user types -v
displayVersionText :: IO ()
displayVersionText =
  TIO.putStrLn $
    "Decimal time clock that maps your day to 1000 decimal minutes, "
      <> "version 1.0.0 "
      <> createPlatformText

-- | If any other command line argument other than -v or --version is given we show help
displayValidArgs :: IO ()
displayValidArgs = TIO.putStrLn "Valid arguments are: -e, -v, --version"

-- | Process args or continue with running the machine
{-# INLINE runClockProcess #-}
runClockProcess :: [String] -> IO ()
runClockProcess = \case
  []            -> runClock False
  ["-e"]        -> runClock True
  ["-v"]        -> displayVersionText
  ["--version"] -> displayVersionText
  _             -> displayValidArgs
  where
    runClock e = do
      let i = ClockState e Nothing Nothing
      runT_ $
        zonedTime
          ~> M.mapping (`updateCurrentDateWithZonedTime` i)
          ~> M.mapping localTimeToDecimal
          ~> M.mapping formatTime
          ~> displayTimeText

main :: IO ()
main = getArgs >>= runClockProcess
