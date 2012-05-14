{-# LANGUAGE ScopedTypeVariables #-}

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


parse :: Monad m => BitParserT m a -> BL.ByteString -> m (Either BitParserError a)
parse m bs = evalStateT (runErrorT m)  . bsToBits $ bs

