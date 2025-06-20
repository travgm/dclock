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
import System.Console.ANSI (hideCursor, showCursor)
import Control.Exception (bracket_)
import Data.Time (ZonedTime, getZonedTime)
import Control.Monad.IO.Class (liftIO)
import System.Info (arch, os)
import Data.Function(fix)
import Options.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified PrettyPrinter as Pretty (formatTime, displaySingleLine, spinner)
import qualified DecimalTime as DT (
      localTimeToDecimal,
      setCurrentDate,
      checkTimeStatus)
import Types (ClockState( .. ), DecimalTime( .. ), RunMode(..))

data Config = Config
    { extended :: Bool
    , mode     :: RunMode
    , alarm    :: Maybe Integer
    }

data Command
  = Version
  | Run Config

-- | Parser for command line arguments
parser :: Parser Command
parser = versionCmd <|> runCmd
  where
    versionCmd = flag'
        Version
        ( long "version"
            <> short 'v'
            <> help "Show version information"
        )

runCmd :: Parser Command
runCmd =
  Run
    <$> ( Config
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
                  <> help "Watch mode"
              )
            <*> (optional . option auto)
              ( long "alarm"
                  <> short 'a'
                  <> metavar "TIME"
                  <> help "Set alarm for decimal time (0-1000, only valid with watch mode)"
              )
        )

-- | Get platform information for version string
createPlatformText :: T.Text
createPlatformText = "(" <> T.pack arch <> "-" <> T.pack os <> ")"

-- | Show version information if the user types -v or --version
displayVersionText :: IO ()
displayVersionText =
  TIO.putStrLn $
    "Decimal time clock that maps your day to 1000 decimal minutes, "
      <> "version 1.0.1 "
      <> createPlatformText

-- | If any other command line argument other than -v or --version, -e
-- or -w is given we show help
displayValidArgs :: IO ()
displayValidArgs = TIO.putStrLn "Valid arguments are: -e, -w, -v, --version"

-- | Output the valid decimal time (consumer)
displayTimeText :: RunMode -> ProcessT IO T.Text ()
displayTimeText mode' = construct $ do
  t <- await
  liftIO $ Pretty.displaySingleLine mode' t

-- | Retrieve initial time and create our process (producer)
zonedTime :: ProcessT IO k ZonedTime
zonedTime = construct $ do
  zt <- liftIO getZonedTime
  yield zt

main :: IO ()
main = execParser opts >>= run
  where
    opts = info
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
            SingleRun -> runClock (extended config') (alarm config') (mode config') >> TIO.putStrLn ""
            Watch     -> watchClock (extended config') (alarm config') (mode config')

        runClock :: Bool -> Maybe Integer -> RunMode -> IO ()
        runClock e alarm' m = do
            let state = ClockState e Nothing Nothing (fmap DecimalTime alarm')
            runT_ $
                zonedTime
                ~> M.mapping (`DT.setCurrentDate` state)
                ~> M.mapping DT.localTimeToDecimal
                ~> M.mapping (\a -> Pretty.formatTime (DT.checkTimeStatus a) a)
                ~> displayTimeText m

        watchClock :: Bool -> Maybe Integer -> RunMode -> IO ()
        watchClock extended' alarm'' m = bracket_ 
            hideCursor
            showCursor 
            (TIO.putStrLn "Press Ctrl-C to exit\n" >>
                fix (\loop -> runClock extended' alarm'' m >> Pretty.spinner >> loop))