module Data.HexStringSpec where

import           Data.HexString ( hexString
                                , hexString'
                                , fromBytes
                                , toBytes )

import qualified Data.ByteString.Char8 as BS8

import           Test.Hspec

spec :: Spec
spec = do
  describe "when constructing a hex string" $ do
    it "should accept strings that fall within a valid range" $
      hexString (BS8.pack "0123456789abcdef") `shouldBe` hexString (BS8.pack "0123456789abcdef")

    it "should reject strings outside the range" $ do
      putStrLn (show (hexString (BS8.pack "/"))) `shouldThrow` anyErrorCall
      putStrLn (show (hexString (BS8.pack ":"))) `shouldThrow` anyErrorCall
      putStrLn (show (hexString (BS8.pack "`"))) `shouldThrow` anyErrorCall
      putStrLn (show (hexString (BS8.pack "g"))) `shouldThrow` anyErrorCall

    it "should return nothing when rejecting" $
      (hexString' (BS8.pack "/")) `shouldBe` Nothing

  describe "when interpreting a hex string" $ do
    it "should convert the hex string properly when interpreting as bytes" $
      toBytes (hexString (BS8.pack "ffff")) `shouldBe` BS8.pack "\255\255"
    it "should convert bytes to the proper hex string" $
      fromBytes (BS8.pack "\255\255") `shouldBe` hexString (BS8.pack "ffff")
