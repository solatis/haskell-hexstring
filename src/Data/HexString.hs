module Data.HexString where

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as BS8
import qualified Data.ByteString.Base16 as BS16

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- | Data type representing a HexString.
data HexString
  = HexString !BS.ByteString
  deriving ( Show, Eq, Ord )

-- | Access to the raw binary data this HexString represents
getBinary :: HexString -> BS.ByteString
getBinary (HexString bs) = bs

-- | Create new HexString based on raw binary data
setBinary :: BS.ByteString -> HexString
setBinary = HexString

-- | Converts `BS.ByteString` to a `HexString`
decodeByteString :: BS.ByteString -> HexString
decodeByteString = HexString . fst . BS16.decode

-- | Converts a `T.Text` representation to a `HexString`
decodeText :: T.Text -> HexString
decodeText = decodeByteString . TE.encodeUtf8

-- | Converts a `String` representation to a `HexString`
decodeString :: String -> HexString
decodeString = decodeByteString . BS8.pack

-- | Converts a `HexString` to a `BS.ByteString`
encodeByteString :: HexString -> BS.ByteString
encodeByteString = BS16.encode . getBinary

-- | Converts a `HexString` to a `T.Text` representation
encodeText :: HexString -> T.Text
encodeText = TE.decodeUtf8 . encodeByteString

-- | Converts a `HexString` to a `String` representation
encodeString :: HexString -> String
encodeString = BS8.unpack . encodeByteString
