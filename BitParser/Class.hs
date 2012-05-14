module BitParser.Class where

import BitParser.Types
import BitParser.Util
import Data.Bits
import Data.Word
import Data.Int

class BitParsable a where
    bget :: BitParser a
    bgets :: Int -> BitParser a


instance BitParsable Bool where
    bget    = getBit
    bgets 1 = getBit
    bgets n = err "instance BitParsable.bgets (Bool): n must be 1"

instance BitParsable Word8 where
    bget = getUnsafeN 8
    bgets n
        | n > 8 || n < 0 = err "instance BitParsable.bgets (Word8): n must be between 0 and 8"
        | otherwise = getUnsafeN n


instance BitParsable Word16 where
    bget = getUnsafeN 16
    bgets n
        | n > 16 || n < 0 = err "instance BitParsable.bgets (Word16): n must be between 0 and 16"
        | otherwise = getUnsafeN n


instance BitParsable Word32 where
    bget = getUnsafeN 32
    bgets n
        | n > 32 || n < 0 = err "instance BitParsable.bgets (Word32): n must be between 0 and 32"
        | otherwise = getUnsafeN n


instance BitParsable Word64 where
    bget = getUnsafeN 64
    bgets n
        | n > 64 || n < 0 = err "instance BitParsable.bgets (Word64): n must be between 0 and 64"
        | otherwise = getUnsafeN n

instance BitParsable Int8 where
    bget = getUnsafeN 8
    bgets n
        | n > 8 || n < 0 = err "instance BitParsable.bgets (Int8): n must be between 0 and 8"
        | otherwise = getUnsafeN n

instance BitParsable Int16 where
    bget = getUnsafeN 16
    bgets n
        | n > 16 || n < 0 = err "instance BitParsable.bgets (Int16): n must be between 0 and 16"
        | otherwise = getUnsafeN n

instance BitParsable Int32 where
    bget = getUnsafeN 32
    bgets n
        | n > 32 || n < 0 = err "instance BitParsable.bgets (Int32): n must be between 0 and 32"
        | otherwise = getUnsafeN n

instance BitParsable Int64 where
    bget = getUnsafeN 64
    bgets n
        | n > 64 || n < 0 = err "instance BitParsable.bgets (Int64): n must be between 0 and 64"
        | otherwise = getUnsafeN n


