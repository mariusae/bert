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
  ( DispatchResult(..)
    -- ** Serve
    -- $example
  , serve
  ) where

import Control.Concurrent (forkIO)
import Control.Monad.Trans (liftIO)
import Network.BERT.Transport (Transport, withTransport, servet, recvt, sendt)
import Data.ByteString.Lazy.Char8 as C
import Data.BERT (Term(..))
import Text.Printf (printf)

data DispatchResult
  = Success Term
  | NoSuchModule
  | NoSuchFunction
  | Undesignated String
    deriving (Eq, Show, Ord)

-- | Serve from the given transport (forever), handling each request
-- with the given dispatch function in a new thread.
serve :: Transport 
      -> (String -> String -> [Term] -> IO DispatchResult)
      -> IO ()
serve transport dispatch =
  servet transport $ \t ->
    (forkIO $ withTransport t $ handleCall dispatch) >> return ()

handleCall dispatch = recvt >>= handle
  where
    handle (TupleTerm [AtomTerm "info", AtomTerm "stream", _]) =
      sendErr "server" 0 "BERTError" "streams are unsupported" []
    handle (TupleTerm [AtomTerm "info", AtomTerm "cache", _]) =
      recvt >>= handle  -- Ignore caching requests.
    handle (TupleTerm [
             AtomTerm "call", AtomTerm mod, 
             AtomTerm fun, ListTerm args]) = do
      res <- liftIO $ dispatch mod fun args
      case res of
        Success term -> 
          sendt $ TupleTerm [AtomTerm "reply", term]
        NoSuchModule ->
          sendErr "server" 1 "BERTError" 
                  (printf "no such module \"%s\"" mod :: String) []
        NoSuchFunction ->
          sendErr "server" 2 "BERTError" 
                  (printf "no such function \"%s\"" fun :: String) []
        Undesignated detail ->
          sendErr "server" 0 "HandlerError" detail []

    sendErr etype ecode eclass detail backtrace = 
      sendt $ TupleTerm [
        AtomTerm "error", 
        TupleTerm [
          AtomTerm etype, IntTerm ecode, BinaryTerm . C.pack $ eclass, 
          ListTerm $ Prelude.map (BinaryTerm . C.pack) backtrace]]

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
