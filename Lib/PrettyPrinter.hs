{-# LANGUAGE OverloadedStrings #-}
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
-----------------------------------------------------------------------------
module PrettyPrinter (
      renderTimeText
    , formatTime
    , displaySingleLine
    , spinner
) where

import Types (
        ClockState( .. )
      , DecimalTime( .. )
      , ValidDecimalTime( .. )
      , TimeStatus( .. )
      , RunMode(..)
      , extendedFlag
      , currentDate
      , decimalTime
      , alarmTime)
import Data.Time (LocalTime)
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import System.Console.ANSI (clearLine, setCursorColumn)
import System.IO (hFlush, stdout)
import Control.Concurrent (threadDelay)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Time.Format as Time

{-# INLINE renderTimeText #-}
renderTimeText :: ClockState -> T.Text -> T.Text
renderTimeText s e =
  "Decimal time: " <> case s ^. decimalTime of
    Just (ValidDecimalTime (DecimalTime 0)) -> "NEW" <> e
    Just (ValidDecimalTime (DecimalTime t)) -> T.pack (show t) <> e
    Nothing -> "Invalid time"

-- | Format the output of the validation
--
-- prop> fmt (Right $ ValidDecimalTime (DecimalTime 1000)) == "Decimal time: NEW"
-- prop> fmt (Right $ ValidDecimalTime (DecimalTime 500)) == "Decimal time: 500"
-- prop> fmt (Right $ ValidDecimalTime (DecimalTime 333)) == "Decimal time: 333"
-- prop> fmt (Left "Time must be between 0 and 1000") == "Decimal time: Time must be between 0 and 1000"
{-# INLINE formatTime #-}
formatTime :: TimeStatus -> Either String ClockState -> T.Text
formatTime status state = case (status, state) of
  (Error err, _) ->
    "Decimal time: " <> T.pack err
  (AlarmReached _, Right s) ->
    baseTimeText s <> " ALARM!\BEL"
  (Normal, Right s) ->
    baseTimeText s
  (_, _) ->
    "Invalid state"
  where
    baseTimeText s = renderTimeText s (extendedInfo s)
    
    {-# INLINE extendedInfo #-}
    extendedInfo :: ClockState -> T.Text
    extendedInfo s =
      if s ^. extendedFlag
        then case s ^. currentDate of
          Just date -> " (" <> T.pack (fmtTime date) <> ")"
          Nothing -> ""
        else ""

    {-# INLINE fmtTime #-}
    fmtTime :: LocalTime -> String
    fmtTime = Time.formatTime Time.defaultTimeLocale "%Y-%m-%d"

-- | Used when we are in WatchClock to update the same line
displaySingleLine :: RunMode -> T.Text -> IO ()
displaySingleLine mode s = do
  clearLine
  setCursorColumn (case mode of Watch -> 2; SingleRun -> 0)
  TIO.putStr s
  hFlush stdout

-- | Spinner for watch mode
spinner :: IO ()
spinner =
  sequence_
    [ putStr ('\r' : c : "") >> hFlush stdout >> threadDelay 250000
      | c <- "-\\|/"
    ]
