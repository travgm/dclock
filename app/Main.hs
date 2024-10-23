{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Travis Montoya 2024
-- License     :  MIT
-- Maintainer  :  trav@hexproof.sh
-- Stability   :  stable
--
-- Convert time from the systems monotonic clock to decimal time. Viewing the
-- day with usable time left we use 1000 as a number counting down from
-- midnight.
--
-- Originally written for the Watchy https://git.sr.ht/~jochen/Calculateur
-- Re-written in Haskell
-----------------------------------------------------------------------------

module Main where

import Data.Time
import Data.Machine
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type Seconds = Days
type Days = Double
type DecimalTime = Int

-- | Pure functions used to calculate the decimal time from Data.Time.getZonedTime
--
-- D = ⌊1000 - (1000 * (H * 3600 + M * 60 + S) / 86400)⌋
-- Where D is decimal time, H is hour, M is minute, S is second
--
-- prop> sec (TimeOfDay 0 0 0) == 0.0 
-- prop> sec (TimeOfDay 1 0 0) == 3600.0 
-- prop> sec (TimeOfDay 24 0 0) == 86400.0
{-# INLINE sec #-}
sec :: TimeOfDay -> Seconds
sec (TimeOfDay !h !m !s) = ((*3600) h' + (*60) m') + s'
  where
    h' = fromIntegral h
    m' = fromIntegral m
    s' = realToFrac s

-- Get fraction of day
-- prop> frac (TimeOfDay 0 0 0) == 0.0
-- prop> frac (TimeOfDay 12 0 0) == 0.5
{-# INLINE frac #-}
frac :: TimeOfDay -> Days
frac = (/ secDay) . sec
  where
    secDay = 24 * 60 * 60 :: Seconds
    {-# INLINE secDay #-}

-- Convert fraction of day to decimal time
-- prop> dec (TimeOfDay 0 0 0) == 1000
-- prop> dec (TimeOfDay 12 0 0) == 500
-- prop> dec (TimeOfDay 16 0 0) == 333
-- +++ OK, passed 1 test.
{-# INLINE dec #-}
dec :: TimeOfDay -> DecimalTime
dec = round . (1000 -) . (* 1000) . frac

-- | The process used below to calculate decimal minutes from the system clock utilizes the machines
-- package to construct a compositional monadic pipeline. A simple way to integrate monadic processing
-- of pure functions with IO.
--
-- This approach will allow for us to enhance the solution in the future with further processing.
--
zTod :: ZonedTime -> TimeOfDay
zTod = localTimeOfDay . zonedTimeToLocalTime

-- Retrieve initial time
getZt :: ProcessT IO k ZonedTime
getZt = construct $ do
  zt <- liftIO getZonedTime
  yield zt

fmtOut :: Int -> T.Text
fmtOut m = T.pack $ "Decimal time: " ++ show m  

result :: ProcessT IO T.Text ()
result = construct $ do
    text <- await
    liftIO $ TIO.putStrLn text

-- Run the machine 
main :: IO ()
main = runT_ $ 
  getZt
    ~> mapping zTod
    ~> mapping dec
    ~> mapping fmtOut
    ~> result

