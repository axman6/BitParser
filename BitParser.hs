{-# LANGUAGE ScopedTypeVariables #-}

module BitParser where

import qualified Data.ByteString.Lazy as BL
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity
import Control.Applicative
import Data.Bits
import Data.Word

import BitParser.Types
import BitParser.Util
import BitParser.Class


parse :: Monad m => BitParserT m a -> BL.ByteString -> m (BitParserResult a)
parse m bs = evalStateT (runErrorT m) st
    where st = BPS bs 0 0


parseState :: Monad m => BitParserT m a -> BL.ByteString -> m (BitParserResultState a)
parseState m bs = runStateT (runErrorT m) st
    where st = BPS bs 0 0
