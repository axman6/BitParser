module BitParser.Util where

import BitParser.Types

import Data.Bits
import Data.Word
import qualified Data.ByteString.Lazy as BL
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity
import Control.Applicative


getBit :: BitParser Bool
getBit = do
    bits <- get
    case bits of
        [] -> err "getBit: ran out of bits"
        (x:xs) ->  do
            put xs
            return x

getBits :: Int -> BitParser [Bool]
getBits n = do
    bits <- get
    let (xs,ys) = splitAt n bits
    put ys
    return xs

getUnsafeN :: Bits a => Int -> BitParser a
getUnsafeN n = createFromLower n <$> getExactly n

getN :: Bits a => Int -> BitParser a
getN n
    | n > bitSize x || n < 0 = err $ "getN: n > bitSize for type"
    | otherwise = (createFromLower n <$> getExactly n) `asTypeOf` return x
    where x = undefined :: Bits a => a

getExactly :: Int -> BitParser [Bool]
getExactly n = do
    bits <- get
    xs <- getBits n
    when (length xs /= n) $ err' bits $ "getExactly: could not get exactly " ++ show n ++ " bits"
    return xs

match :: Bits a => a -> BitParser a
match x = do
    bits <- get
    xs <- getExactly (bitSize x)
    when (xs /= xbits) $ err' bits $ "match:\nExpected: " ++ showBits xbits ++ "\nRecieved: " ++ showBits xs
    return x
    where xbits = toBits x

matchHigh :: Bits a => Int -> a -> BitParser a
matchHigh n x
    | n > bitSize x = err $ "matchHigh: n > bitSize x"
    |otherwise = do
        bits <- get
        xs <- getExactly n
        when (xs /= xbits) $ err' bits $ "matchHigh:\nExpected: " ++ showBits xbits 
                                                ++ "\nRecieved: " ++ showBits xs
        return $ createFromUpper n xs
    where xbits = take n $ toBits x

matchHighShifted :: Bits a => Int -> a -> BitParser a
matchHighShifted n x = do
    y <- matchHigh n x
    return (y `shiftR` (bitSize x - n))


matchLow :: Bits a => Int -> a -> BitParser a
matchLow n x
    | n > bitSize x = err $ "matchLow: n > bitSize x"
    |otherwise = do
        bits <- get
        xs <- getExactly n
        when (xs /= xbits) $ err' bits $ "matchLow:\nExpected: " ++ showBits xbits 
                                               ++ "\nRecieved: " ++ showBits xs
        return $ createFromLower n xs
    where xbits = drop (bitSize x - n) $ toBits x

err :: String -> BitParser a
err str = do
    bits <- get
    err' bits str

err' :: [Bool] -> String -> BitParser a
err' bits str = throwError $ BPE bits str

bsToBits :: BL.ByteString -> [Bool]
bsToBits bs = BL.unpack bs >>= toBits

createFromLower :: Bits a => Int -> [Bool] -> a
createFromLower n xs = foldl (\n (p,b) -> if b then setBit n p else n) 0 $ zip [n-1,n-2..0] xs

createFromUpper :: Bits a => Int -> [Bool] -> a
createFromUpper n xs = (foldl (\n (p,b) -> if b then setBit n p else n) 0 $ zip [size,size-1..0] xs) `asTypeOf` x
    where x = undefined :: Bits a => a
          size = bitSize x - 1

toBits :: Bits a => a -> [Bool]
toBits n = map (testBit n) [size, size-1..0] where
    size = bitSize n - 1

showBits :: [Bool] -> String
showBits []         = ""
showBits (True:xs)  = '1':showBits xs
showBits (False:xs) = '0':showBits xs