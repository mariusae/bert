-- |
-- Module      : Network.BERT
-- Copyright   : (c) marius a. eriksen 2009
-- 
-- License     : BSD3
-- Maintainer  : marius@monkey.org
-- Stability   : experimental
-- Portability : GHC
-- 
-- BERT-RPC client (<http://bert-rpc.org/>). See
-- "Network.BERT.Transport" and "Network.BERT.RPC" for more details.
module Network.BERT
  ( module Network.BERT.Transport
  , module Network.BERT.Client
  , module Network.BERT.Server
  -- * Example
  -- $example
  ) where

import Network.BERT.Transport
import Network.BERT.Client
import Network.BERT.Server

-- $example
-- 
-- > t <- fromURI "bert://localhost:8000"
-- > r <- call t "errorcalc" "add" ([123, 300]::[Int])
-- > case r of 
-- >   Right res -> print (res::Int)
-- >   Left e    -> print e
