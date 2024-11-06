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
  , extendedFlag
  , decimalTime
  , currentDate
  , Lens
  , Lens''
  , (><)
) where

import Data.Time (LocalTime)
import qualified Control.Lens as L

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
newtype ValidDecimalTime = ValidDecimalTime DecimalTime
  deriving (Show, Eq)

data ClockState = ClockState
  { _extendedFlag :: Bool,
    _decimalTime  :: Maybe ValidDecimalTime,
    _currentDate  :: Maybe LocalTime
  }
  deriving (Eq, Show)

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens'' s a = L.Lens s s a a

extendedFlag :: Lens'' ClockState Bool
extendedFlag k (ClockState e d c) = (\e' -> ClockState e' d c) <$> k e

decimalTime :: Lens'' ClockState (Maybe ValidDecimalTime)
decimalTime k (ClockState e d c) = (\d' -> ClockState e d' c) <$> k d

currentDate :: Lens'' ClockState (Maybe LocalTime)
currentDate k (ClockState e d c) = (\c' -> ClockState e d c') <$> k c