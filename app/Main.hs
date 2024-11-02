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
import Control.Concurrent (threadDelay)
import System.Info (arch, os)
import System.Environment(getArgs)
import Data.Function(fix)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified PrettyPrinter as Pretty (formatTime, displaySingleLine)
import qualified DecimalTime as DT (
      localTimeToDecimal,
      updateCurrentDateWithZonedTime,
      ClockState(ClockState))
import Types (Config(..), RunMode(SingleRun, Watch))

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
displayValidArgs = TIO.putStrLn "Valid arguments are: -e, -w, -v, --version"

-- | Output the valid decimal time (consumer)
displayTimeText :: ProcessT IO T.Text ()
displayTimeText = construct $ await >>= liftIO . Pretty.displaySingleLine

-- | Retrieve initial time and create our process (producer)
zonedTime :: ProcessT IO k ZonedTime
zonedTime = construct $ do
  zt <- liftIO getZonedTime
  yield zt

-- | Process args or continue with running the machine
runClockProcess :: [String] -> IO ()
runClockProcess = \case
    []            -> runWith $ Config False SingleRun
    ["-e"]        -> runWith $ Config True SingleRun
    ["-w"]        -> runWith $ Config False Watch
    ["-w", "-e"]  -> runWith $ Config True Watch
    ["-e", "-w"]  -> runWith $ Config True Watch
    ["-v"]        -> displayVersionText
    ["--version"] -> displayVersionText
    _             -> displayValidArgs
  where
    runWith :: Config -> IO ()
    runWith config = case mode config of
      SingleRun -> runClock (extended config) >> TIO.putStrLn ""
      Watch     -> watchClock (extended config)

    runClock :: Bool -> IO ()
    runClock e = do
      let state = DT.ClockState e Nothing Nothing
      runT_ $
        zonedTime
          ~> M.mapping (`DT.updateCurrentDateWithZonedTime` state)
          ~> M.mapping DT.localTimeToDecimal
          ~> M.mapping Pretty.formatTime
          ~> displayTimeText

    watchClock :: Bool -> IO ()
    watchClock extended' = 
      TIO.putStrLn "Press Ctrl-C to exit" >>
      fix (\loop -> runClock extended' >> threadDelay 1000000 >> loop)

main :: IO ()
main = getArgs >>= runClockProcess
