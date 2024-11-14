{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
-----------------------------------------------------------------------------
module Types (
    Seconds(..)
  , Days(..)
  , DecimalTime(..)
  , ValidDecimalTime(..)
  , ClockState(..)
  , TimeStatus(..)
  , extendedFlag
  , decimalTime
  , currentDate
  , alarmTime
  , Lens
  , Lens''
  , (><)
) where

import Data.Time (LocalTime)
import qualified Control.Lens as L
import System.Console.ANSI (ConsoleIntensity(NormalIntensity))

-- | Infix operator for range checking where lower bound is 0
infixr 5 ><
(><) :: Integer -> Integer -> Bool
(><) x y = x >= 0 && x <= y

-- | Decimal time types
newtype Seconds = Seconds Double
  deriving (Eq, Ord, Num, Fractional, Real, RealFrac)

newtype Days = Days Double
  deriving (Eq, Ord, Num, Fractional, Real, RealFrac)

newtype DecimalTime = DecimalTime Integer
  deriving (Eq, Show, Ord, Num, Enum, Real, Integral)

-- | Validation for our decimal time values
newtype ValidDecimalTime = ValidDecimalTime { unVDT :: DecimalTime }
  deriving (Show, Eq)

data TimeStatus
  = Normal
  | AlarmReached DecimalTime
  | Error String

data ClockState = ClockState
  { _extendedFlag :: Bool,
    _decimalTime  :: Maybe ValidDecimalTime,
    _currentDate  :: Maybe LocalTime,
    _alarmTime    :: Maybe DecimalTime
  }
  deriving (Eq, Show)

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens'' s a = L.Lens s s a a

extendedFlag :: Lens'' ClockState Bool
extendedFlag k (ClockState e d c a) = (\e' -> ClockState e' d c a) <$> k e

decimalTime :: Lens'' ClockState (Maybe ValidDecimalTime)
decimalTime k (ClockState e d c a) = (\d' -> ClockState e d' c a) <$> k d

currentDate :: Lens'' ClockState (Maybe LocalTime)
currentDate k (ClockState e d c a) = (\c' -> ClockState e d c' a) <$> k c

alarmTime :: Lens'' ClockState (Maybe DecimalTime)
alarmTime k (ClockState e d c a) = (\a' -> ClockState e d c a') <$> k a