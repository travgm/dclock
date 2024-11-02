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

import Data.Machine as M (
       ProcessT, 
       await, 
       construct, 
       mapping, 
       runT_, 
       yield, 
       (~>))
import Data.Time (ZonedTime, getZonedTime)
import Control.Monad.IO.Class (liftIO)
import System.Info ( arch, os )
import System.Environment(getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified PrettyPrinter as Pretty (formatTime)
import qualified DecimalTime as DT (
      localTimeToDecimal,
      updateCurrentDateWithZonedTime,
      ClockState(ClockState))

-- | Get platform information for version string
{-# INLINE createPlatformText #-}
createPlatformText :: T.Text
createPlatformText = "(" <> T.pack arch <> "-" <> T.pack os <> ")"

-- | Show version information if the user types -v
{-# INLINE displayVersionText #-}
displayVersionText :: IO ()
displayVersionText =
  TIO.putStrLn $
    "Decimal time clock that maps your day to 1000 decimal minutes, "
      <> "version 1.0.0 "
      <> createPlatformText

-- | If any other command line argument other than -v or --version is given we show help
{-# INLINE displayValidArgs #-}
displayValidArgs :: IO ()
displayValidArgs = TIO.putStrLn "Valid arguments are: -e, -v, --version"

-- | Output the valid decimal time (consumer)
{-# INLINE displayTimeText #-}
displayTimeText :: ProcessT IO T.Text ()
displayTimeText = construct $ await >>= liftIO . TIO.putStrLn

-- | Retrieve initial time and create our process (producer)
{-# INLINE zonedTime #-}
zonedTime :: ProcessT IO k ZonedTime
zonedTime = construct $ do
  zt <- liftIO getZonedTime
  yield zt

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
      let i = DT.ClockState e Nothing Nothing
      runT_ $
        zonedTime
          ~> M.mapping (`DT.updateCurrentDateWithZonedTime` i)
          ~> M.mapping DT.localTimeToDecimal
          ~> M.mapping Pretty.formatTime
          ~> displayTimeText

main :: IO ()
main = getArgs >>= runClockProcess
