{-# LANGUAGE ForeignFunctionInterface #-}
module Curly.Core.Security.SHA256(hashlazy) where

import Definitive
import qualified Foreign.C as F
import qualified Foreign.Ptr as F
import qualified Foreign.Marshal.Alloc as F
import Data.ByteString.Lazy (toChunks)
import Data.ByteString (useAsCStringLen,packCStringLen)

data SHA256_CTX = SHA256_CTX

foreign import ccall "sha256_init" sha256_init :: F.Ptr SHA256_CTX -> IO ()
foreign import ccall "sha256_update" sha256_update :: F.Ptr SHA256_CTX -> F.Ptr F.CChar -> F.CInt -> IO ()
foreign import ccall "sha256_final" sha256_final :: F.Ptr SHA256_CTX -> F.Ptr F.CChar -> IO ()

hashlazy :: Bytes -> Chunk
hashlazy bs = by thunk $ do
  F.allocaBytes 32 $ \ret -> do
    F.allocaBytes 108 $ \ctx -> do
      sha256_init ctx
      for_ (toChunks bs) $ \c -> do
        useAsCStringLen c $ \(s,sz) -> do
          sha256_update ctx s (fromIntegral sz)
      sha256_final ctx ret
    packCStringLen (ret,32)
