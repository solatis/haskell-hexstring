module Data.HexString ( HexString (..)
                      , decodeText
                      , decodeString
                      , encodeText
                      , encodeString ) where

import           Control.Applicative         ((<$>))

import qualified Data.ByteString.Base16.Lazy as BS16L (decode, encode)
import qualified Data.ByteString.Lazy.Char8  as BSL8
import qualified Data.ByteString.Lazy        as BSL

import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as TE

import qualified Data.Binary                 as B (Binary, decode, encode, get,
                                                   put)
import qualified Data.Binary.Get             as B (getRemainingLazyByteString)
import qualified Data.Binary.Put             as B (putLazyByteString)

-- | Data type representing a HexString.
data HexString
  = HexString BSL.ByteString
  deriving ( Show, Eq, Ord )

-- | Allows us to convert to and from a `B.Binary` representation. Always
--   assumes that the entire binary string that is fed to `Binary.decode`
--   represents the hex string.
instance B.Binary HexString where
  get                = HexString <$> B.getRemainingLazyByteString
  put (HexString bs) = B.putLazyByteString bs

-- | Converts a `T.Text` representation to a `HexString`
decodeText :: T.Text -> HexString
decodeText = decodeByteString . BSL.fromStrict . TE.encodeUtf8

-- | Converts a `String` representation to a `HexString`
decodeString :: String -> HexString
decodeString = decodeByteString . BSL8.pack

-- | Converts a `HexString` to a `T.Text` representation
encodeText :: HexString -> T.Text
encodeText = TE.decodeUtf8 . BSL.toStrict . encodeByteString

-- | Converts a `HexString` to a `String` representation
encodeString :: HexString -> String
encodeString = BSL8.unpack . encodeByteString



-- | Internal function that converts a `HexString` to a `BSL.ByteString`
encodeByteString :: HexString -> BSL.ByteString
encodeByteString = BS16L.encode . B.encode

-- | Internal funcion that converts `BSL.ByteString` to a `HexString`
decodeByteString :: BSL.ByteString -> HexString
decodeByteString = B.decode . fst . BS16L.decode
