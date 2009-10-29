{-# LANGUAGE OverlappingInstances #-}
-- |
-- Module      : Data.BERT.Term
-- Copyright   : (c) marius a. eriksen 2009
-- 
-- License     : BSD3
-- Maintainer  : marius@monkey.org
-- Stability   : experimental
-- Portability : GHC
-- 
-- Define BERT terms and their binary encoding & decoding.
module Data.BERT.Term
  ( Term(..)
  ) where

import Control.Monad (forM_, replicateM)
import Control.Applicative ((<$>))
import Data.Bits (shiftR, (.&.))
import Data.Binary (Binary(..), Word8)
import Data.Binary.Put (Put, putWord8, putWord16be, 
                        putWord32be, putLazyByteString)
import Data.Binary.Get (Get, getWord8, getWord16be, getWord32be,
                        getLazyByteString)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Printf (printf)

data Term
  -- Simple (erlang) terms:
  = IntTerm        Int
  | FloatTerm      Float
  | AtomTerm       String
  | TupleTerm      [Term]
  | BytelistTerm   ByteString
  | ListTerm       [Term]
  | BinaryTerm     ByteString
  | BigintTerm     Integer
  | BigbigintTerm  Integer
    -- Complex terms:
  | NilTerm
  | BoolTerm       Bool
  | DictionaryTerm [(Term, Term)]
    -- TODO: time, regex
    deriving (Show, Eq, Ord)

-- Binary encoding & decoding.
instance Binary Term where
  put term = putWord8 131 >> putTerm term
  get      = getWord8 >>= \version ->
               case version of 
                 131 -> getTerm
                 _ -> fail "wrong version"

-- | Binary encoding of a single term (without header)
putTerm (IntTerm value) = tag 98 >> put32i value
putTerm (FloatTerm value) =
  tag 99 >> (putL . C.pack . pad $ printf "%15.15e" value)
  where
    pad s = s ++ replicate (31 - (length s)) '\0'

putTerm (AtomTerm value)
  | len < 256 = tag 100 >> put16i len >> (putL $ C.pack value)
  | otherwise = fail "BERT atom too long (>= 256)"
  where
    len = length value
putTerm (TupleTerm value)
  | len < 256 = tag 104 >> put8i len  >> forM_ value putTerm
  | otherwise = tag 105 >> put32i len >> forM_ value putTerm
  where
    len = length value
putTerm (BytelistTerm value)
  | len < 65536 = tag 107 >> put16i len >> putL value
  | otherwise = do  -- too big: encode as a list.
      tag 108
      put32i len
      forM_ (B.unpack value) $ \v -> do 
        tag 97
        putWord8 v
  where 
    len = B.length value
putTerm (ListTerm value)
  | len == 0 = putNil  -- this is mentioend in the BERT spec.
  | otherwise= do
      tag 108
      put32i $ length value
      forM_ value putTerm
      putNil
  where 
    len = length value
    putNil = putWord8 106

putTerm (BinaryTerm value) = tag 109 >> (put32i $ B.length value) >> putL value
putTerm (BigintTerm value) = tag 110 >> putBigint put8i value
putTerm (BigbigintTerm value) = tag 111 >> putBigint put32i value
-- Complex terms:
putTerm NilTerm = putTerm $ TupleTerm [AtomTerm "bert", AtomTerm "nil"]
putTerm (BoolTerm value) =
  putTerm $ TupleTerm [AtomTerm "bert", AtomTerm value']
  where
    value' = if value then "true" else "false"
putTerm (DictionaryTerm value) =
  putTerm $ TupleTerm [ AtomTerm "bert"
                      , AtomTerm "dict"
                      , ListTerm $ map (\(k, v) -> TupleTerm [k, v]) value
                      ]
-- | Binary decoding of a single term (without header)
getTerm = do
  tag <- get8i
  case tag of
    97  -> IntTerm <$> get8i
    98  -> IntTerm <$> get32i
    99  -> getL 31 >>= return . FloatTerm . read . C.unpack
    100 -> get16i >>= getL >>= return . AtomTerm . C.unpack
    104 -> get8i >>= getN >>= tupleTerm
    105 -> get32i >>= getN >>= tupleTerm 
    106 -> return $ ListTerm []
    107 -> get16i >>= getL >>= return . BytelistTerm
    108 -> get32i >>= getN >>= return . ListTerm
    109 -> get32i >>= getL >>= return . BinaryTerm
    110 -> getBigint get8i >>= return . BigintTerm . fromIntegral
    111 -> getBigint get32i >>= return . BigintTerm . fromIntegral
  where
    getN n = replicateM n getTerm
    tupleTerm [AtomTerm "bert", AtomTerm "true"]  = return $ BoolTerm True
    tupleTerm [AtomTerm "bert", AtomTerm "false"] = return $ BoolTerm False
    tupleTerm [AtomTerm "bert", AtomTerm "dict", ListTerm kvs] =
      mapM toTuple kvs >>= return . DictionaryTerm
      where
        toTuple (TupleTerm [k, v]) = return $ (k, v)
        toTuple _ = fail "invalid dictionary"

    tupleTerm xs = return $ TupleTerm xs

putBigint putter value = do
  putter len  -- TODO: verify size?
  if value < 0
    then put8i 1
    else put8i 0
  putL $ B.pack $ map (fromIntegral . digit) [0..len-1]
  where
    value'    = abs value
    len       = ceiling $ logBase 256 (fromIntegral $ value' + 1)
    digit pos = (value' `shiftR` (8 * pos)) .&. 0xFF

getBigint getter = do
  len   <- fromIntegral <$> getter
  sign  <- get8i
  bytes <- getL len
  multiplier <- 
    case sign of 
      0 -> return 1
      1 -> return (-1)
      _ -> fail "Invalid sign byte"
  return $ (*) multiplier
         $ foldl (\s (n, d) -> s + d*(256^n)) 0
         $ zip [0..len-1] (map fromIntegral $ B.unpack bytes)

put8i :: (Integral a) => a -> Put
put8i = putWord8 . fromIntegral
put16i :: (Integral a) => a -> Put
put16i = putWord16be . fromIntegral
put32i :: (Integral a) => a -> Put
put32i = putWord32be . fromIntegral
putL = putLazyByteString

get8i  = fromIntegral <$> getWord8
get16i = fromIntegral <$> getWord16be
get32i = fromIntegral <$> getWord32be
getL :: (Integral a) => a -> Get ByteString
getL = getLazyByteString . fromIntegral

tag :: Word8 -> Put
tag which = putWord8 which
