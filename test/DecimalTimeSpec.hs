module DecimalTimeSpec where

import Test.Hspec
import Test.QuickCheck
import DecimalTime
import Types
import Control.Lens ((^.), (?~))
import Data.Time

instance Arbitrary TimeOfDay where
  arbitrary =
    TimeOfDay
      <$> choose (0, 23)
      <*> choose (0, 59)
      <*> (fromInteger <$> choose (0, 59))

instance Arbitrary TimeZone where
  arbitrary = TimeZone <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Day where
  arbitrary = ModifiedJulianDay <$> arbitrary

instance Arbitrary LocalTime where
  arbitrary = LocalTime <$> arbitrary <*> arbitrary

instance Arbitrary ZonedTime where
  arbitrary = do
    timeZone <- arbitrary
    localTime <- arbitrary
    return $ ZonedTime localTime timeZone

makeTestState :: TimeOfDay -> ClockState
makeTestState tod =
  ClockState False Nothing (Just $ LocalTime (fromGregorian 2024 1 1) tod)

spec :: Spec
spec = do
  describe "localTimeToDecimal" $ do
    it "converts midnight to 1000" $ do
      let state = makeTestState (TimeOfDay 0 0 0)
      case localTimeToDecimal state of
        Right s -> s ^. decimalTime `shouldBe` Just (ValidDecimalTime (DecimalTime 1000))
        Left err -> expectationFailure $ "Expected Right but got Left: " ++ err

    it "converts noon to 500" $ do
      let state = makeTestState (TimeOfDay 12 0 0)
      case localTimeToDecimal state of
        Right s -> s ^. decimalTime `shouldBe` Just (ValidDecimalTime (DecimalTime 500))
        Left err -> expectationFailure $ "Expected Right but got Left: " ++ err

    it "converts teatime to 333" $ do
      let state = makeTestState (TimeOfDay 16 0 0)
      case localTimeToDecimal state of
        Right s -> s ^. decimalTime `shouldBe` Just (ValidDecimalTime (DecimalTime 333))
        Left err -> expectationFailure $ "Expected Right but got Left: " ++ err

    it "fails when currentDate is Nothing" $ do
      let state = ClockState False Nothing Nothing
      case localTimeToDecimal state of
        Left _ -> return ()
        Right _ -> expectationFailure "Expected Left but got Right"

    it "always produces value between 0 and 1000" $ property $ \tod ->
      let state = makeTestState tod
       in case localTimeToDecimal state of
            Right s -> case s ^. decimalTime of
              Just (ValidDecimalTime (DecimalTime t)) ->
                t >= 0 && t <= 1000
              _ -> False
            Left _ -> False

    it "preserves extended flag" $ property $ \extended tod ->
      let state = (makeTestState tod) {_extendedFlag = extended}
       in case localTimeToDecimal state of
            Right s -> s ^. extendedFlag == extended
            Left _ -> True

  describe "updateCurrentDateWithZonedTime returns proper date" $ do
    it "returns the date from the ZonedTime" $ property $ \tod zt ->
      let state = makeTestState tod
          localTime = zonedTimeToLocalTime zt
          updatedState = updateCurrentDateWithZonedTime zt state
       in updatedState ^. currentDate == Just localTime
