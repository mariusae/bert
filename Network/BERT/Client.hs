-- |
-- Module      : Network.BERT.Client
-- Copyright   : (c) marius a. eriksen 2009
-- 
-- License     : BSD3
-- Maintainer  : marius@monkey.org
-- Stability   : experimental
-- Portability : GHC
-- 
-- BERT-RPC client (<http://bert-rpc.org/>). This implements the
-- client RPC call logic.

module Network.BERT.Client
  ( Call, call 
  ) where

import Data.BERT (Term(..), Packet(..), BERT(..))
import Network.BERT.Transport (Transport, withTransport, sendt, recvt)

data Error 
  = ClientError String
  | ServerError Term
    deriving (Show, Ord, Eq)

-- | Convenience type for @call@
type Call a = IO (Either Error a)

-- | Call the @{mod, func, args}@ synchronously on the endpoint
-- defined by @transport@, returning the results of the call or an
-- error.
call :: (BERT a, BERT b)
     => Transport 
     -> String 
     -> String
     -> [a]
     -> Call b
call transport mod fun args = 
  withTransport transport $ do
    sendt $ TupleTerm [AtomTerm "call", AtomTerm mod, AtomTerm fun, 
                       ListTerm $ map showBERT args]
    recvt >>= handle
  where
    handle (TupleTerm [AtomTerm "reply", reply]) =
      return $ either (const . Left $ ClientError "decode failed") Right
             $ readBERT reply
    handle (TupleTerm (AtomTerm "info":_)) = 
      recvt >>= handle  -- We don't yet handle info directives.
    handle t@(TupleTerm (AtomTerm "error":_)) =
      return $ Left . ServerError $ t
