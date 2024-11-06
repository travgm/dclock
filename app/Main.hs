{-# LANGUAGE OverloadedStrings #-}
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
import Data.Function(fix)
import Options.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified PrettyPrinter as Pretty (formatTime, displaySingleLine)
import qualified DecimalTime as DT (
      localTimeToDecimal,
      setCurrentDate)
import Types (ClockState( .. ))

data RunMode = SingleRun | Watch

data Config = Config
    { extended :: Bool
    , mode     :: RunMode
    }

data Command
  = Version
  | Run Config

-- | Parser for command line arguments
parser :: Parser Command
parser = versionCmd <|> runCmd
  where
    versionCmd =
      flag'
        Version
        ( long "version"
            <> short 'v'
            <> help "Show version information"
        )

    runCmd =
      fmap Run $
        Config
          <$> switch
            ( long "extended"
                <> short 'e'
                <> help "Show extended information including date"
            )
          <*> flag
            SingleRun
            Watch
            ( long "watch"
                <> short 'w'
                <> help "Watch mode, view as a realtime decimal clock (updates every second)"
            )

-- | Get platform information for version string
createPlatformText :: T.Text
createPlatformText = "(" <> T.pack arch <> "-" <> T.pack os <> ")"

-- | Show version information if the user types -v or --version
displayVersionText :: IO ()
displayVersionText =
  TIO.putStrLn $
    "Decimal time clock that maps your day to 1000 decimal minutes, "
      <> "version 1.0.0 "
      <> createPlatformText

-- | If any other command line argument other than -v or --version, -e
-- or -w is given we show help
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

main :: IO ()
main = execParser opts >>= run
  where
    opts =
      info
        (parser <**> helper)
        ( fullDesc
            <> progDesc "Decimal time clock that maps your day to 1000 decimal minutes"
            <> header "dclock - decimal time clock"
        )
   
    run :: Command -> IO ()
    run Version = displayVersionText
    run (Run config) = runWith config
      where
        runWith :: Config -> IO ()
        runWith config' = case mode config' of
          SingleRun -> runClock (extended config') >> TIO.putStrLn ""
          Watch     -> watchClock (extended config')

        runClock :: Bool -> IO ()
        runClock e = do
          let state = ClockState e Nothing Nothing
          runT_ $
            zonedTime
              ~> M.mapping (`DT.setCurrentDate` state)
              ~> M.mapping DT.localTimeToDecimal
              ~> M.mapping Pretty.formatTime
              ~> displayTimeText

        watchClock :: Bool -> IO ()
        watchClock extended' = 
          TIO.putStrLn "Press Ctrl-C to exit\n" >>
          fix (\loop -> runClock extended' >> threadDelay 1000000 >> loop)