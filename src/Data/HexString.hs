module Data.HexString ( HexString
                      , hexString
                      , toHex
                      , fromHex
                      , asText ) where

import           Data.Word                   (Word8)

import qualified Data.ByteString.Base16 as BS16 (decode, encode)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL

import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE

import qualified Data.Binary            as B (Binary, decode, encode)

-- | Represents a Hex string. Guarantees that all characters it contains
--   are valid hex characters.
data HexString =
  HexString BS.ByteString
  deriving ( Show, Eq, Ord )

-- | Smart constructor which validates that all the text are actually
--   hexadecimal characters.
hexString :: BS.ByteString -> HexString
hexString bs =
  let isValidHex :: Word8 -> Bool
      isValidHex c
        | (48 <= c) && (c < 58)  = True
        | (97 <= c) && (c < 103) = True
        | otherwise              = False

  in if   BS.all isValidHex bs
     then (HexString bs)
     else error ("Not a valid hex string: " ++ show bs)

-- | Converts a 'B.Binary' to a 'HexString' value
toHex :: B.Binary a  => a -> HexString
toHex = hexString . BS16.encode . BSL.toStrict . B.encode

-- | Converts a 'HexString' to a 'B.Binary' value
fromHex :: B.Binary a => HexString -> a
fromHex (HexString bs) = B.decode . BSL.fromStrict . fst . BS16.decode $ bs

-- | Access to a 'T.Text' representation of the 'HexString'
asText :: HexString -> T.Text
asText (HexString bs) = TE.decodeUtf8 bs
