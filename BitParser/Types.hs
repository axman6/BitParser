
module BitParser.Types where

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity

data BitParserError = BPE [Bool] String

instance Show BitParserError where
    show (BPE _ str) = str

type BitParserT m a = ErrorT BitParserError (StateT [Bool] m) a

type BitParser a = BitParserT Identity a

instance Error BitParserError where
    noMsg = BPE [] ""
    strMsg s = BPE [] s


