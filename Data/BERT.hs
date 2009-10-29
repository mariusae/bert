{-# LANGUAGE OverlappingInstances #-}
{-# OPTIONS -XTypeSynonymInstances #-}
-- |
-- Module      : Data.BERT
-- Copyright   : (c) marius a. eriksen 2009
-- 
-- License     : BSD3
-- Maintainer  : marius@monkey.org
-- Stability   : experimental
-- Portability : GHC
-- 
-- BERT (Erlang terms + RPC) implementation.
--   - <http://bert-rpc.org/>
--   - <http://erlang.org/doc/apps/erts/erl_ext_dist.html>
module Data.BERT 
  ( BERT(..)
  , Term(..)
  , Packet(..)
  ) where

import Control.Monad.Error
import Data.Char (chr)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Map (Map)
import qualified Data.Map as Map

import Data.BERT.Term (Term(..))
import Data.BERT.Packet (Packet(..))

class BERT a where
  showBERT :: a -> Term
  readBERT :: Term -> (Either String a)

-- Herein are some instances for common Haskell data types. To do
-- anything more complicated, you should make your own instance.

instance BERT Term where
  showBERT = id
  readBERT = return . id

instance BERT Int where
  showBERT = IntTerm
  readBERT (IntTerm value) = return value

instance BERT Bool where
  showBERT = BoolTerm
  readBERT (BoolTerm x) = return x

instance BERT Integer where
  showBERT = BigbigintTerm
  readBERT (BigintTerm x) = return x
  readBERT (BigbigintTerm x) = return x
  readBERT _ = fail "Invalid integer type"

instance BERT Float where
  showBERT = FloatTerm
  readBERT (FloatTerm value) = return value
  readBERT _ = fail "Invalid floating point type"

instance BERT String where
  showBERT = BytelistTerm . C.pack
  readBERT (BytelistTerm x) = return $ C.unpack x
  readBERT (BinaryTerm x) = return $ C.unpack x
  readBERT (AtomTerm x) = return x
  readBERT (ListTerm xs) = mapM readBERT xs >>= return . map chr
  readBERT _ = fail "Invalid string type"

instance BERT ByteString where
  showBERT = BytelistTerm
  readBERT (BytelistTerm value) = return value
  readBERT _ = fail "Invalid bytestring type"

instance (BERT a) => BERT [a] where
  showBERT xs = ListTerm $ map showBERT xs
  readBERT (ListTerm xs) = mapM readBERT xs
  readBERT _ = fail "Invalid list type"

instance (BERT a, BERT b) => BERT (a, b) where
  showBERT (a, b) = TupleTerm [showBERT a, showBERT b]
  readBERT (TupleTerm [a, b]) = liftM2 (,) (readBERT a) (readBERT b)
  readBERT _ = fail "Invalid tuple(2) type"

instance (BERT a, BERT b, BERT c) => BERT (a, b, c) where
  showBERT (a, b, c) = TupleTerm [showBERT a, showBERT b, showBERT c]
  readBERT (TupleTerm [a, b, c]) = 
    liftM3 (,,) (readBERT a) (readBERT b) (readBERT c)
  readBERT _ = fail "Invalid tuple(3) type"

instance (BERT a, BERT b, BERT c, BERT d) => BERT (a, b, c, d) where
  showBERT (a, b, c, d) = 
    TupleTerm [showBERT a, showBERT b, showBERT c, showBERT d]
  readBERT (TupleTerm [a, b, c, d]) = 
    liftM4 (,,,) (readBERT a) (readBERT b) (readBERT c) (readBERT d)
  readBERT _ = fail "Invalid tuple(4) type"

instance (Ord k, BERT k, BERT v) => BERT (Map k v) where
  showBERT m = DictionaryTerm 
             $ map (\(k, v) -> (showBERT k, showBERT v)) (Map.toList m)
  readBERT (DictionaryTerm kvs) = 
    mapM (\(k, v) -> liftM2 (,) (readBERT k) (readBERT v)) kvs >>=
      return . Map.fromList
  readBERT _ = fail "Invalid map type"

