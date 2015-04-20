module Data.HexStringSpec where

import           Data.HexString ( hexString
                                , toHex
                                , fromHex
                                , asText )

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text             as T

import qualified Data.Binary as B ( encode )

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
