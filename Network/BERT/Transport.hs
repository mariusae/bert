{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module      : Network.BERT.Transport
-- Copyright   : (c) marius a. eriksen 2009
-- 
-- License     : BSD3
-- Maintainer  : marius@monkey.org
-- Stability   : experimental
-- Portability : GHC
-- 
-- Transport for BERT-RPC client. Creates a transport from a URI,
-- leaving flexibility for several underlying transport
-- implementations. The current one is over sockets (TCP), with the
-- URI scheme bert, eg: <bert://localhost:9000>
-- 
-- The TCP transport will create a new connection for every request
-- (every block of 'withTransport'), which seems to be what the
-- current server implementations expect. It'd be great to have
-- persistent connections, however.

module Network.BERT.Transport
  ( Transport, fromURI
  -- ** Transport monad
  , TransportM, withTransport
  , sendt, recvt
  ) where

import Control.Monad.State (
  StateT, MonadIO, MonadState, runStateT, 
  modify, gets, liftIO)
import Network.URI (URI(..), URIAuth(..), parseURI)
import Network.Socket (
  Socket(..), Family(..), SockAddr(..), SocketType(..), 
  connect, socket, sClose)
import Data.Maybe (fromJust)
import Data.Binary (encode, decode)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import qualified Network.DNS.Client as DNS
import qualified Network.Socket.ByteString.Lazy as LS

import Data.BERT.Term (Term(..))
import Data.BERT.Packet (Packet(..), packets)

-- | Defines a transport endpoint. Create with 'fromURI'.
data Transport
  = TcpTransport SockAddr
    deriving (Show, Eq)

data TransportState 
  = TransportState {
      state_packets :: [Packet]
    , state_socket  :: Socket
    }

newtype TransportM a 
  = TransportM (StateT TransportState IO a)
    deriving (Monad, MonadIO, MonadState TransportState)

-- | Create a transport from the given URI.
fromURI :: String -> IO Transport
fromURI = fromURI_ . fromJust . parseURI

fromURI_ uri@(URI { uriScheme = "bert:"
                  , uriAuthority = Just URIAuth 
                                   { uriRegName = host
                                   , uriPort = ':':port}}) =
  fromHostPort host (fromIntegral . read $ port)

resolve host = do
  r <- DNS.resolve DNS.A host
  case r of
    Left error -> fail $ show error
    Right [] -> fail "No DNS A records!"
    Right (((_, DNS.RRA (addr:_))):_) -> return addr

fromHostPort host port = do
  resolve host >>= return . TcpTransport . SockAddrInet (fromIntegral port)

-- | Execute the given transport monad action in the context of the
-- passed transport.
withTransport :: Transport -> TransportM a -> IO a
withTransport (TcpTransport sa) (TransportM m) = do
  sock <- socket AF_INET Stream 0
  connect sock sa
  ps <- LS.getContents sock >>= return . packets
  (result, _) <- runStateT m TransportState 
                             { state_packets = ps 
                             , state_socket  = sock}
  sClose sock
  return result

-- | Send a term (inside the transport monad)
sendt :: Term -> TransportM ()
sendt t = do
  sock <- gets state_socket
  liftIO $ LS.sendAll sock $ encode (Packet t)
  return ()

-- | Receive a term (inside the transport monad)
recvt :: TransportM Term
recvt = do
  ps <- gets state_packets
  modify $ \state -> state { state_packets = drop 1 ps }
  let Packet t = head ps
  return t