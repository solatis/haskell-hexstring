module Data.HexStringSpec where

import           Data.HexString

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text             as T

import           Test.Hspec

spec :: Spec
spec = do
  describe "when decoding hex data" $ do
    it "should be able to parse basic hex data" $ do
      (getBinary . decodeByteString) (BS8.pack "ffff") `shouldBe` BS8.pack "\255\255"
      (getBinary . decodeString) "ffff"                `shouldBe` BS8.pack "\255\255"
      (getBinary . decodeText) (T.pack "ffff")         `shouldBe` BS8.pack "\255\255"

    it "should be able to recode basic hex data to different formats" $
      let hex = BS8.pack "ffff"
      in do
        (encodeText . decodeByteString) hex       `shouldBe` T.pack "ffff"
        (encodeString . decodeByteString) hex     `shouldBe` "ffff"
        (encodeByteString . decodeByteString) hex `shouldBe` BS8.pack "ffff"
