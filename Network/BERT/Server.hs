-- |
-- Module      : Network.BERT.Server
-- Copyright   : (c) marius a. eriksen 2009
-- 
-- License     : BSD3
-- Maintainer  : marius@monkey.org
-- Stability   : experimental
-- Portability : GHC
-- 
-- BERT-RPC server (<http://bert-rpc.org/>). This implements the
-- client RPC call/reply logic. Only synchronous requests are
-- supported at this time.

module Network.BERT.Server 
  ( -- ** Serve
    -- $example
    serve
  ) where

import Control.Concurrent (forkIO)
import Control.Monad.Trans (liftIO)
import Data.BERT.Term (Term(..))
import Network.BERT.Transport (Transport, withTransport, servet, recvt, sendt)
import Data.ByteString.Lazy.Char8 as C

-- | Serve from the given transport (forever), handling each request
-- with the given dispatch function in a new thread.
serve :: Transport 
      -> (String -> String -> [Term] -> IO (Either String Term)) 
      -> IO ()
serve transport dispatch =
  servet transport $ \t -> 
    (forkIO $ withTransport t $ handleCall dispatch) >> return ()

handleCall dispatch =
  recvt >>= handle
  where
    handle (TupleTerm [AtomTerm "info", AtomTerm "stream", _]) =
      sendt $ TupleTerm [       -- Streams are unsupported at this time.
               AtomTerm "error", IntTerm 0, 
               BinaryTerm C.empty, 
               BinaryTerm $ C.pack "streams not supported"]
    handle (TupleTerm [AtomTerm "info", AtomTerm "cache", _]) =
      recvt >>= handle  -- Ignore caching requests.
    handle (TupleTerm [
             AtomTerm "call", AtomTerm mod, 
             AtomTerm fun, ListTerm args]) = do
      res <- liftIO $ dispatch mod fun args
      case res of
        Left error -> 
          sendt $ TupleTerm [
                   AtomTerm "error", IntTerm 0, 
                   BinaryTerm C.empty, BinaryTerm $ C.pack error]
        Right term -> 
          sendt $ TupleTerm [AtomTerm "reply", term]

-- $example
-- 
-- To serve requests, create a transport and call 'serve' with a
-- dispatch function.
-- 
-- > main = do
-- >   t <- fromHostPort "" 8080
-- >   serve t dispatch
-- >
-- > dispatch "calc" "add" [IntTerm a, IntTerm b] = 
-- >   return $ Right $ IntTerm (a + b)
-- > dispatch _ _ _ = do
-- >   return $ Left "no such m/f!"
