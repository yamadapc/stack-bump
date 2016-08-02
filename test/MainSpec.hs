{-# LANGUAGE BangPatterns #-}
module MainSpec
    ( spec
    )
  where

import Data.Either
import Test.Hspec
import Test.QuickCheck

import StackBump.Main

spec :: Spec
spec =
    describe "readBumpType :: String -> Either String BumpType" $ do
        it "doesn't throw" $ property $ \as -> do
            let !bt = readBumpType as
            True `shouldBe` True

        it "readBumpType []" $
            isLeft (readBumpType []) `shouldBe` True

        it "readBumpType [\"other\"]" $
            isLeft (readBumpType ["other"]) `shouldBe` True

        it "readBumpType [\"other\", \"asdfasdf\"]" $
            isLeft (readBumpType ["other", "asdfasdf"]) `shouldBe` True

        it "readBumpType [\"minor\"]" $
            readBumpType ["minor"] `shouldBe` Right BumpTypeMinor

        it "readBumpType [\"major\"]" $
            readBumpType ["major"] `shouldBe` Right BumpTypeMajor

        it "readBumpType [\"patch\"]" $
            readBumpType ["patch"] `shouldBe` Right BumpTypePatch

        it "readBumpType [\"other\"]" $
            readBumpType ["other", "10"] `shouldBe` Right (BumpTypeOther 10)
