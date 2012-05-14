
module BitParser.Types where

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity
import qualified Data.ByteString.Lazy as BL
import Data.Int

data BitParserError = BPE BitParserState String 

data BitParserState = BPS {currentBS :: !BL.ByteString, bitsParsed :: !Int64, bytePos :: !Int}
    deriving Show

emptyBPS = BPS BL.empty 0 0

instance Show BitParserError where
    show (BPE _ str) = str

type BitParserT m a = ErrorT BitParserError (StateT BitParserState m) a

type BitParser a = BitParserT Identity a

instance Error BitParserError where
    noMsg = BPE emptyBPS ""
    strMsg s = BPE emptyBPS s

type BitParserResult a = Either BitParserError a
type BitParserResultState a = (Either BitParserError a, BitParserState)



err :: Monad m => String -> BitParserT m a
err str = do
    bits <- get
    err' bits str

err' :: Monad m =>  BitParserState -> String -> BitParserT m a
err' bits str = throwError $ BPE bits str

