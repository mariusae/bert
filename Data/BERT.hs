-- |
-- Module      : Data.BERT
-- Copyright   : (c) marius a. eriksen 2009
-- 
-- License     : BSD3
-- Maintainer  : marius@monkey.org
-- Stability   : experimental
-- Portability : GHC
-- 
-- BERT (Erlang terms) implementation. See <http://bert-rpc.org/> and
-- <http://erlang.org/doc/apps/erts/erl_ext_dist.html> for more
-- details.
module Data.BERT 
  ( BERT(..)
  , Term(..)
  , Packet(..)
  ) where

import Data.BERT.Term (Term(..), BERT(..))
import Data.BERT.Packet (Packet(..))
