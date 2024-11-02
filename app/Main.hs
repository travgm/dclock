{-# LANGUAGE OverloadedStrings #-}
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
-----------------------------------------------------------------------------
module Main where

import DecimalTime ( 
      displayTimeText,
      formatTime,
      localTimeToDecimal,
      updateCurrentDateWithZonedTime,
      zonedTime,
      ClockState(ClockState))
import Data.Machine as M ( runT_, (~>), mapping )
import System.Info ( arch, os )
import System.Environment(getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

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
      let i = DecimalTime.ClockState e Nothing Nothing
      runT_ $
        DecimalTime.zonedTime
          ~> M.mapping (`DecimalTime.updateCurrentDateWithZonedTime` i)
          ~> M.mapping DecimalTime.localTimeToDecimal
          ~> M.mapping DecimalTime.formatTime
          ~> DecimalTime.displayTimeText

main :: IO ()
main = getArgs >>= runClockProcess
