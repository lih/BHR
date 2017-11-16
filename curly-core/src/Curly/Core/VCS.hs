{-# LANGUAGE DeriveGeneric #-}
module Curly.Core.VCS where

import Curly.Core
import Curly.Core.Library
import Curly.Core.VCS.Diff
import Curly.Core.Security
import Definitive
import Language.Format
import qualified Crypto.Hash.SHA256 as SHA256
import System.Process (readProcess)

newtype Hash = Hash Chunk
             deriving (Eq,Ord)
hashData :: Bytes -> Hash
hashData b = Hash (SHA256.hashlazy b)
instance Show Hash where
  show (Hash h) = pretty h
instance Read Hash where
  readsPrec _ = readsParser (readable <&> \(Pretty h) -> Hash h)
instance Serializable Hash where
  encode (Hash h) = h^.chunkBuilder
instance Format Hash where
  datum = Hash<$>getChunk 32

type Commit = Compressed (Patch LibraryID Metadata,Maybe Hash)
type Branches = Map String ((PublicKey,String):+:Hash)
data VCCommand = PublishLibrary LibraryID Bytes
               | PublishSource LibraryID (Signed String)
               | SetBranches PublicKey (Signed Branches)
               | ListBranches PublicKey (WithResponse (Maybe (Signed Branches)))
               | CreateCommit Commit (WithResponse Hash)
               | GetCommit Hash (WithResponse (Maybe Commit))
               | GetLibrary LibraryID (WithResponse (Maybe Bytes))
               | GetSource LibraryID (WithResponse (Maybe (Signed String)))
               deriving (Generic,Show)
instance Serializable VCCommand; instance Format VCCommand

data VCSBackend = VCSB_Curly String PortNumber
                | VCSB_None
                deriving (Eq,Ord)
instance Show VCSBackend where
  show (VCSB_Curly h p) = "curly-vc://"+h+":"+show p
  show VCSB_None = "none"
instance Read VCSBackend where
  readsPrec _ = readsParser $ backend
    where backend = curlyBackend <+? fill VCSB_None (several "none")
          curlyBackend = do
            several "curly-vc://" <+? single '@'
            liftA2 VCSB_Curly
              (many1' (noneOf ":") <&> \x -> if x=="_" then "127.0.0.1" else x)
              (option' 5402 (single ':' >> number))
              
curlyVCSBackend :: VCSBackend
curlyVCSBackend = fromMaybe (getDefaultVCS^.thunk) (matches Just readable (envVar "" "CURLY_VCS"))
  where getDefaultVCS = do
          lns <- map words . lines <$> readProcess "/usr/lib/curly/default-vcs" [] ""
          return $ case lns of
            ([h,p]:_) -> VCSB_Curly h (fromInteger $ read p)
            _ -> VCSB_None
            
curlyPublisher :: String
curlyPublisher = envVar "" "CURLY_PUBLISHER"

getVCSBranches :: MonadIO m => String -> m Branches
getVCSBranches name = do
  u <- lookup name <$> getKeyStore
  case (curlyVCSBackend,u) of
    (VCSB_Curly h p,Just (_,pub,_,_,_)) -> do
      conn <- liftIO $ connectTo h p
      map (fromMaybe zero) $ runConnection Just True conn $ do
        bs <- exchange (ListBranches pub)
        maybe zero (return . unsafeExtractSigned) bs
    _ -> return zero
