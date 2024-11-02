{-# LANGUAGE OverloadedStrings #-}
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
module PrettyPrinter (
      renderTimeText
    , fmtTime
) where

import Types
  ( ClockState (..),
    DecimalTime (..),
    ValidDecimalTime (..),
    decimalTime)
import Data.Time (LocalTime)
import Control.Lens ((^.))
import qualified Data.Text as T
import qualified Data.Time.Format as Time

{-# INLINE renderTimeText #-}
renderTimeText :: ClockState -> T.Text -> T.Text
renderTimeText s e =
  "Decimal time: " <> case s ^. decimalTime of
    Just (ValidDecimalTime (DecimalTime 0)) -> "NEW" <> e
    Just (ValidDecimalTime (DecimalTime t)) -> T.pack (show t) <> e
    Nothing -> "Invalid time"

{-# INLINE fmtTime #-}
fmtTime :: LocalTime -> String
fmtTime = Time.formatTime Time.defaultTimeLocale "%Y-%m-%d"