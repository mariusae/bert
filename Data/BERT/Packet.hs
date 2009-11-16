-- |
-- Module      : Data.BERT.Packet
-- Copyright   : (c) marius a. eriksen 2009
-- 
-- License     : BSD3
-- Maintainer  : marius@monkey.org
-- Stability   : experimental
-- Portability : GHC
-- 
-- BERP (BERT packets) support.
module Data.BERT.Packet 
  ( Packet(..)
  , fromPacket
  , packets
  ) where

import Control.Monad (liftM)
import Data.ByteString.Lazy as L
import Data.Binary (Binary(..), Get(..), encode, decode)
import Data.Binary.Put (putWord32be, putLazyByteString)
import Data.Binary.Get (getWord32be, getLazyByteString, runGet, runGetState)

import Data.BERT.Term
import Data.BERT.Types (Term(..))

-- | A single BERP. Little more than a wrapper for a term.
data Packet
  = Packet Term
    deriving (Show, Ord, Eq)

fromPacket (Packet t) = t

instance Binary Packet where
  put (Packet term) = 
    putWord32be (fromIntegral len) >> putLazyByteString encoded
    where encoded = encode term
          len     = L.length encoded

  get = getPacket

getPacket =
  liftM fromIntegral getWord32be >>= 
  getLazyByteString              >>= 
  return . Packet . decode

-- | From a lazy bytestring, return a (lazy) list of packets. This is
-- convenient for parsing a stream of adjacent packets. (Eg. by using
-- some form of @getContents@ to get a @ByteString@ out of a data
-- source).
packets :: L.ByteString -> [Packet]
packets b
  | L.null b = []
  | otherwise = p:packets b' 
      where (p, b', _) = runGetState getPacket b 0
