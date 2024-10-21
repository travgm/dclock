{-# LANGUAGE OverloadedStrings #-}

import Data.Time
import Data.Machine
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- | Pure functions used to calculate the decimal time from Data.Time.getZonedTime
--
-- D = ⌊1000 - (1000 * (H * 3600 + M * 60 + S) / 86400)⌋
-- Where D is decimal time, H is hour, M is minute, S is second
--
secondsSinceMidnight :: TimeOfDay -> Double
secondsSinceMidnight (TimeOfDay h m s) = fromIntegral (h * 3600 + m * 60) + realToFrac s

fractionOfDayPassed :: TimeOfDay -> Double
fractionOfDayPassed = (/ totalSecondsInDay) . secondsSinceMidnight
  where
    totalSecondsInDay = 24 * 60 * 60 :: Double

timeToDecimalMinutes :: TimeOfDay -> Int
timeToDecimalMinutes = round . (1000 -) . (* 1000) . fractionOfDayPassed

-- | The process used below to calculate decimal minutes from the system clock utilizes the machines
-- package to construct a compositional monadic pipeline. A simple way to integrate monadic processing
-- of pure functions with IO.
--
-- This approach will allow for us to enhance the solution in the future with further processing.
--
zonedToTimeOfDay :: ZonedTime -> TimeOfDay
zonedToTimeOfDay = localTimeOfDay . zonedTimeToLocalTime

-- Retrieve initial time
getZonedTime' :: ProcessT IO k ZonedTime
getZonedTime' = construct $ do
  zt <- liftIO getZonedTime
  yield zt

formatOutput :: Int -> T.Text
formatOutput m = T.pack $ "Decimal time: " ++ show m  

outputResult :: ProcessT IO T.Text ()
outputResult = construct $ do
    text <- await
    liftIO $ TIO.putStrLn text

-- Run the machine 
main :: IO ()
main = runT_ $ 
  getZonedTime'
    ~> mapping zonedToTimeOfDay
    ~> mapping timeToDecimalMinutes
    ~> mapping formatOutput
    ~> outputResult

