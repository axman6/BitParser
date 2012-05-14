module BitParser.Util where

import BitParser.Types

import Data.Bits
import Data.Word
import Data.Int
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity
import Control.Applicative


getBit :: Monad m => BitParserT m Bool
getBit = do
    state <- get
    xs <- getBits 1
    case xs of
        [] -> err' state $ "getBit: not enough bits"
        [x] -> return x


getBits :: Monad m => Int64 -> BitParserT m [Bool]
getBits n = do
    BPS bs red bpos <- get
    let resBits = take (fromIntegral n) . drop bpos . bsToBits $ bs
        (x,y) = divMod n 8
        toDrop = x + if bpos == 0 then 0 else 1
        bs' = BL.drop (fromIntegral toDrop) bs
    put (BPS bs' (red + n) ((bpos + fromIntegral y) `mod` 8))
    return resBits

getUnsafeN :: (Bits a, Monad m) => Int64 -> BitParserT m a
getUnsafeN n = createFromLower n `liftM` getExactly n

getN :: (Bits a, Monad m) => Int64 -> BitParserT m a
getN n
    | n > fromIntegral (bitSize x) || n < 0 = err $ "getN: n > bitSize for type"
    | otherwise = (createFromLower n `liftM` getExactly n) `asTypeOf` return x
    where x = undefined :: Bits a => a
-- 
getExactly :: Monad m => Int64 -> BitParserT m [Bool]
getExactly n = do
    state@(BPS bs red bpos) <- get
    xs <- getBits n
    when (fromIntegral (length xs) /= n) $ err' state $ "getExactly: could not get exactly " ++ show n ++ " bits"
    return xs

match :: (Bits a, Monad m) => a -> BitParserT m a
match x = do
    bits <- get
    xs <- getExactly (fromIntegral $ bitSize x)
    when (xs /= xbits) $ err' bits $ "match:\nExpected: " ++ showBits xbits ++ "\nRecieved: " ++ showBits xs
    return x
    where xbits = toBits x

matchHigh :: (Bits a, Monad m) => Int64 -> a -> BitParserT m a
matchHigh n x
    | n > fromIntegral (bitSize x) = err $ "matchHigh: n > bitSize x"
    |otherwise = do
        bits <- get
        xs <- getExactly n
        when (xs /= xbits) $ err' bits $ "matchHigh:\nExpected: " ++ showBits xbits 
                                                ++ "\nRecieved: " ++ showBits xs
        return $ createFromUpper n xs
    where xbits = take (fromIntegral n) $ toBits x

matchHighShifted :: (Bits a, Monad m) => Int64 -> a -> BitParserT m a
matchHighShifted n x = do
    y <- matchHigh n x
    return (y `shiftR` (bitSize x - fromIntegral n))


matchLow :: (Bits a, Monad m) => Int64 -> a -> BitParserT m a
matchLow n x
    | n > fromIntegral (bitSize x) = err $ "matchLow: n > bitSize x"
    |otherwise = do
        bits <- get
        xs <- getExactly n
        when (xs /= xbits) $ err' bits $ "matchLow:\nExpected: " ++ showBits xbits 
                                               ++ "\nRecieved: " ++ showBits xs
        return $ createFromLower n xs
    where xbits = drop (bitSize x - fromIntegral n) $ toBits x


bitsRead :: Monad m => BitParserT m Int64
bitsRead = gets bitsParsed

getBytes' :: Monad m => Int64 -> BitParserT m BS.ByteString
getBytes' n = do
    xs <- getExactly (n*8)
    return . BS.pack . toWord8s $ xs

-- | Skip any remaining partial bits until the next byte in the bytestring,
-- then get the next n bytes. This will usually be more efficient than getBytes'
-- since it doesn't have to create a new ByteString from bits.
-- 
-- If you know that the bytes you're going to read are byte aligned (they don't
-- start half way through a byte), then you should use this instead of getBytes'.
getBytes :: Monad m => Int64 -> BitParserT m BL.ByteString
getBytes n = do
    state@(BPS bs red pos) <- get
    let (s,res) = case pos of
                    0 -> let (xs,ys) = BL.splitAt n bs
                             s = BPS ys (red + fromIntegral pos + (n * 8)) 0
                         in (s,xs)
                    _ -> let (xs,ys) = BL.splitAt n (BL.drop 1 bs)
                             s = BPS ys (red + (n * 8)) 0
                         in (s,xs)
    put s
    when (BL.length res /= n) $ err' state $ "getBytes': could not get " ++ show n ++ " bytes"
    return res
      




bsToBits :: BL.ByteString -> [Bool]
bsToBits bs = BL.unpack bs >>= toBits

createFromLower :: Bits a => Int64 -> [Bool] -> a
createFromLower n xs = foldl (\n (p,b) -> if b then setBit n p else n) 0 $ zip [n'-1,n'-2..0] xs
    where n' = fromIntegral n

createFromUpper :: Bits a => Int64 -> [Bool] -> a
createFromUpper n xs = (foldl (\n (p,b) -> if b then setBit n p else n) 0 $ zip [size,size-1..0] xs) `asTypeOf` x
    where x = undefined :: Bits a => a
          size = bitSize x - 1

toBits :: Bits a => a -> [Bool]
toBits n = map (testBit n) [size, size-1..0] where
    size = bitSize n - 1


toWord8s :: [Bool] -> [Word8]
toWord8s xs = case splitAt 8 xs of
    (ys,[]) -> [createFromUpper 8 ys]
    (ys,zs) -> createFromUpper 8 ys : toWord8s zs

showBits :: [Bool] -> String
showBits []         = ""
showBits (True:xs)  = '1':showBits xs
showBits (False:xs) = '0':showBits xs