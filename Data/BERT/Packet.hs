-- |
-- Module      : Data.BERT.Packet
-- Copyright   : (c) marius a. eriksen 2009
-- 
-- License     : BSD3
-- Maintainer  : marius@monkey.org
-- Stability   : experimental
-- Portability : GHC
-- 
-- BERP support.
module Data.BERT.Packet 
  ( Packet(..)
  ) where

import Control.Monad (liftM)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy as B
import Data.Binary (Binary(..), encode, decode)
import Data.Binary.Put (putWord32be, putLazyByteString)
import Data.Binary.Get (getWord32be, getLazyByteString)

import Data.BERT.Term (Term(..))

data Packet 
  = Packet Term
    deriving (Show, Ord, Eq)

instance Binary Packet where
  put (Packet term) = 
    putWord32be (fromIntegral len) >> putLazyByteString encoded
    where encoded = encode term
          len     = B.length encoded
  get = liftM fromIntegral getWord32be >>= 
        getLazyByteString              >>= 
        return . Packet . decode

