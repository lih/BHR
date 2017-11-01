module IO.Network.Socket (
  PortNumber,SockAddr(..),NameInfoFlag(..),
  listenOn,accept,connect,
  Net.getAddrInfo,Net.getNameInfo
  ) where

import Definitive
import Network.Socket (Socket,AddrInfo,PortNumber,SockAddr(..),NameInfoFlag(..))
import qualified Network.Socket as Net
import System.IO (IOMode(..))
import GHC.IO.Handle

instance Semigroup PortNumber; instance Monoid PortNumber

listenOn :: PortNumber -> IO Socket
listenOn p = Net.socket Net.AF_INET Net.Stream Net.defaultProtocol <*= \s -> do
  Net.setSocketOption s Net.ReuseAddr 1
  Net.bind s (Net.SockAddrInet p Net.iNADDR_ANY)
  Net.listen s 5

accept :: Socket -> IO (Handle,SockAddr)
accept s = Net.accept s >>= \(s',a) -> (,a) <$> Net.socketToHandle s' ReadWriteMode

connect :: AddrInfo -> IO Handle
connect addr = do
  s <- Net.socket (Net.addrFamily addr) Net.Stream (Net.addrProtocol addr)
  Net.connect s (Net.addrAddress addr)
  Net.socketToHandle s ReadWriteMode <*= \h -> hSetBuffering h NoBuffering
  
