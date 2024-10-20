{-# LANGUAGE OverloadedStrings #-}

import Data.Time
import Data.Machine
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Pure functions used to calculate the decimal time from Data.Time.getZonedTime
-- DT = ⌊1000 - (1000 * (HR * 3600 + MIN * 60 + SSEC) / 86400)⌋
-- Where DT is decimal time, HR is hour, MIN is minute, SEC is second
secondsSinceMidnight :: TimeOfDay -> Double
secondsSinceMidnight tod = fromIntegral (todHour tod * 3600 + todMin tod * 60) + realToFrac (todSec tod)

fractionOfDayPassed :: TimeOfDay -> Double
fractionOfDayPassed tod = secondsSinceMidnight tod / sid
  where
    sid = 24 * 60 * 60

timeToDecimalMinutes :: TimeOfDay -> Int
timeToDecimalMinutes tod = round ((1000 :: Double) - (fractionOfDayPassed tod * 1000))

-- The process used below to calculate decimal minutes from the system clock utilizes the machines
-- package to construct a compositional monadic pipeline. A simple way to integrate monadic processing
-- of pure functions with IO. Each pipeline is a morphism in the category machines and I use 'autoM' and
-- 'construct' to lift the pure functions into the MachineT context.
--
-- This approach will allow for us to enhance the solution in the future with further processing.
zonedToTimeOfDay :: ZonedTime -> TimeOfDay
zonedToTimeOfDay = localTimeOfDay . zonedTimeToLocalTime

formatOutput :: Int -> T.Text
formatOutput m = T.pack $ "Decimal time: " ++ show m

getZonedTime' :: ProcessT IO k ZonedTime
getZonedTime' = construct $ do
  zt <- liftIO getZonedTime
  yield zt

outputResult :: ProcessT IO T.Text ()
outputResult = construct $ do
    text <- await
    liftIO $ TIO.putStrLn text

main :: IO ()
main = runT_ $ 
  getZonedTime'
    ~> mapping zonedToTimeOfDay
    ~> mapping timeToDecimalMinutes
    ~> mapping formatOutput
    ~> outputResult