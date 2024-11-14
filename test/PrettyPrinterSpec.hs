{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module PrettyPrinterSpec where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Text as T
import Data.Time
import Types
import Control.Lens ((^.))
import Data.Maybe (isJust, Maybe(..))
import qualified PrettyPrinter as Pretty

instance Arbitrary TimeOfDay where
  arbitrary =
    TimeOfDay
      <$> choose (0, 23)
      <*> choose (0, 59)
      <*> (fromInteger <$> choose (0, 59))

instance Arbitrary ClockState where
  arbitrary = do
    ext <- arbitrary
    dtime <- arbitrary
    date <- arbitrary
    return $ ClockState ext dtime date Nothing

instance Arbitrary ValidDecimalTime where
  arbitrary = ValidDecimalTime . DecimalTime <$> choose (0, 1000)

instance Arbitrary LocalTime where
  arbitrary =
    LocalTime
      <$> (fromGregorian <$> choose (2020, 2025) <*> choose (1, 12) <*> choose (1, 28))
      <*> arbitrary

makeClockState :: Bool -> Maybe LocalTime -> Maybe ValidDecimalTime -> ClockState
makeClockState ext date dtime =
  ClockState ext dtime date Nothing

makeValidTime :: Integer -> ValidDecimalTime
makeValidTime = ValidDecimalTime . DecimalTime

spec :: Spec
spec = do
  describe "renderTimeText" $ do
    it "renders NEW for 0" $ do
      let state = makeClockState False Nothing (Just $ makeValidTime 0)
      Pretty.renderTimeText state "" `shouldBe` "Decimal time: NEW"

    it "renders number for non-zero time" $ do
      let state = makeClockState False Nothing (Just $ makeValidTime 500)
      Pretty.renderTimeText state "" `shouldBe` "Decimal time: 500"

    it "handles Nothing decimal time" $ do
      let state = makeClockState False Nothing Nothing
      Pretty.renderTimeText state "" `shouldBe` "Decimal time: Invalid time"

  describe "formatTime" $ do
    it "formats error message" $ do
        Pretty.formatTime (Error "test error") (Left "test error") 
            `shouldBe` "Decimal time: test error"

    it "formats normal time without extended info" $ do
        let state = makeClockState False Nothing (Just $ makeValidTime 500)
        Pretty.formatTime Normal (Right state) 
            `shouldBe` "Decimal time: 500"

    it "formats time with date when extended" $ do
        let date = LocalTime (fromGregorian 2024 1 1) midnight
            state = makeClockState True (Just date) (Just $ makeValidTime 500)
        Pretty.formatTime Normal (Right state) 
            `shouldBe` "Decimal time: 500 (2024-01-01)"

    it "formats NEW with date when extended" $ do
        let date = LocalTime (fromGregorian 2024 1 1) midnight
            state = makeClockState True (Just date) (Just $ makeValidTime 0)
        Pretty.formatTime Normal (Right state) 
            `shouldBe` "Decimal time: NEW (2024-01-01)"

    it "shows alarm when reached" $ do
        let state = makeClockState False Nothing (Just $ makeValidTime 500)
        Pretty.formatTime (AlarmReached (DecimalTime 500)) (Right state)
            `shouldBe` "Decimal time: 500 ALARM!\BEL"

    it "always starts with 'Decimal time: '" $ property $ \state ->
        T.isPrefixOf "Decimal time: " (Pretty.formatTime Normal $ Right state)