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
  ( Transport, fromURI, fromHostPort
  -- ** Transport monad
  , TransportM, withTransport
  , sendt, recvt
  -- ** Server side
  , servet
  ) where

import Control.Monad (forever)
import Control.Monad.State (
  StateT, MonadIO, MonadState, runStateT, 
  modify, gets, liftIO)
import Network.URI (URI(..), URIAuth(..), parseURI)
import Network.Socket (
  Socket(..), Family(..), SockAddr(..), SocketType(..), 
  SocketOption(..),  connect, socket, sClose, setSocketOption, 
  bindSocket, listen, accept, iNADDR_ANY)
import Data.Maybe (fromJust)
import Data.Binary (encode, decode)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import qualified Network.DNS.Client as DNS
import qualified Network.Socket.ByteString.Lazy as LS
import qualified System.Posix.Signals as Sig

import Data.BERT.Term (Term(..), BERT(..))
import Data.BERT.Packet (Packet(..), packets)

-- | Defines a transport endpoint. Create with 'fromURI'.
data Transport
  = TcpTransport       SockAddr
  | TcpServerTransport Socket
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

-- | Create a (TCP) transport from the given host and port
fromHostPort :: (Integral a) => String -> a -> IO Transport
fromHostPort "" port = 
  return $ TcpTransport 
         $ SockAddrInet (fromIntegral port) iNADDR_ANY
fromHostPort host port = do
  resolve host >>= return . TcpTransport 
                          . SockAddrInet (fromIntegral port)

fromURI_ uri@(URI { uriScheme = "bert:"
                  , uriAuthority = Just URIAuth 
                                   { uriRegName = host
                                   , uriPort = ':':port}}) =
  fromHostPort host (fromIntegral . read $ port)

servet :: Transport -> (Transport -> IO ()) -> IO ()
servet (TcpTransport sa) dispatch = do
  -- Ignore sigPIPE, which can be delivered upon writing to a closed
  -- socket.
  Sig.installHandler Sig.sigPIPE Sig.Ignore Nothing

  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bindSocket sock sa
  listen sock 1

  forever $ do
    (clientsock, _) <- accept sock
    setSocketOption clientsock NoDelay 1
    dispatch $ TcpServerTransport clientsock

resolve host = do
  r <- DNS.resolve DNS.A host
  case r of
    Left error -> fail $ show error
    Right [] -> fail "No DNS A records!"
    Right (((_, DNS.RRA (addr:_))):_) -> return addr

-- | Execute the given transport monad action in the context of the
-- passed transport.
withTransport :: Transport -> TransportM a -> IO a
withTransport (TcpTransport sa) m = do
  sock <- socket AF_INET Stream 0
  connect sock sa
  withTransport_ sock m
withTransport (TcpServerTransport sock) m =
  withTransport_ sock m

withTransport_ sock (TransportM m) = do
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
