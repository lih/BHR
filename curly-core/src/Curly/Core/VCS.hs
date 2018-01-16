{-# LANGUAGE DeriveGeneric, ExistentialQuantification #-}
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

commitHash :: Commit -> Hash
commitHash c = hashData (serialize c)

type Commit = Compressed (Patch LibraryID Metadata,Maybe Hash)
type Branches = Map String ((PublicKey,String):+:Hash)
data VCKey o = LibraryKey LibraryID (WithResponse Bytes)
             | SourceKey LibraryID (WithResponse (Signed String))
             | BranchesKey PublicKey (WithResponse (Signed Branches))
             | CommitKey Hash (WithResponse Commit)
             | OtherKey o
           deriving (Show,Generic)
instance Serializable o => Serializable (VCKey o)
instance Format o => Format (VCKey o)
instance Serializable o => Eq (VCKey o) where a==b = compare a b==EQ
instance Serializable o => Ord (VCKey o) where compare = comparing serialize
instance Functor VCKey where
  map f (OtherKey o) = OtherKey (f o)
  map f (LibraryKey a b) = LibraryKey a b
  map f (SourceKey a b) = SourceKey a b
  map f (CommitKey a b) = CommitKey a b
  map f (BranchesKey a b) = BranchesKey a b

class MonadIO vc => MonadVC vc s | vc -> s where
  vcStore :: Serializable a => s -> (WithResponse a -> VCKey ()) -> a -> vc ()
  vcLoad :: Format a => s -> (WithResponse a -> VCKey ()) -> vc (Maybe a)
  runVC :: vc a -> IO a

newtype File_VC a = File_VC (IO a)
                  deriving (Functor,SemiApplicative,Unit,Applicative)
instance Monad File_VC where join = coerceJoin File_VC
instance MonadIO File_VC where liftIO = File_VC
instance MonadVC File_VC String where
  vcStore base k v = liftIO $ do
    let keyName = pretty (serialize (k WithResponse)^.chunk)
    writeSerial (base+"/"+keyName) v
  vcLoad base k = liftIO $ do
    let keyName = pretty (serialize (k WithResponse)^.chunk)
    try (return Nothing) (Just <$> readFormat (base+"/"+keyName))
  runVC (File_VC io) = io

newtype Client_VC a = Client_VC (IO a)
                  deriving (Functor,SemiApplicative,Unit,Applicative)
instance Monad Client_VC where join = coerceJoin Client_VC
instance MonadIO Client_VC where liftIO = Client_VC
instance MonadVC Client_VC Handle where
  vcStore conn k l = liftIO $ writeHSerial conn ((True,k WithResponse),l)
  vcLoad conn k = liftIO $ try (return Nothing) $ runConnection Just False conn $ do
    exchange (\r -> (False,k (pMaybe r))) >>= maybe zero pure
  runVC (Client_VC io) = io

pMaybe :: WithResponse (Maybe a) -> WithResponse a
pMaybe _ = WithResponse
maybeP :: WithResponse a -> WithResponse (Maybe a)
maybeP _ = WithResponse

vcServer (VCSB_Native _ st run) = do
  (b,k) <- receive
  logLine Verbose ("Received request "+show (b,k))
  if b then case k of
    LibraryKey lid _ -> receive >>= liftIO . run . vcStore st (LibraryKey lid)
    SourceKey lid _ ->  receive >>= liftIO . run . vcStore st (SourceKey lid)
    CommitKey h _ ->    receive >>= liftIO . run . vcStore st (CommitKey h)
    BranchesKey pub _ -> receive >>= liftIO . run . vcStore st (BranchesKey pub)
    OtherKey () -> return ()
    else case k of
    LibraryKey lid t -> sending (maybeP t) =<< liftIO (run $ vcLoad st (LibraryKey lid))
    SourceKey lid t -> sending (maybeP t) =<< liftIO (run $ vcLoad st (SourceKey lid))
    CommitKey h t -> sending (maybeP t) =<< liftIO (run $ vcLoad st (CommitKey h))
    BranchesKey pub t -> sending (maybeP t) =<< liftIO (run $ vcLoad st (BranchesKey pub))
    OtherKey () -> return ()

vcbStore :: (Serializable a,MonadIO m) => VCSBackend -> (WithResponse a -> VCKey ()) -> a -> m ()
vcbStore (VCSB_Native _ st run) k a = liftIO (run (vcStore st k a))
vcbLoad :: (Format a,MonadIO m) => VCSBackend -> (WithResponse a -> VCKey ()) -> m (Maybe a)
vcbLoad (VCSB_Native _ st run) k = liftIO (run (vcLoad st k))
vcbLoadP :: (Format a,MonadIO m) => VCSBackend -> (WithResponse a -> VCKey ()) -> ParserT s m a
vcbLoadP b k = vcbLoad b k >>= maybe zero return

data VCSBackend = forall m s. MonadVC m s => VCSB_Native String s (forall a. m a -> IO a)
                | VCSB_None
instance Eq VCSBackend where a == b = compare a b == EQ
instance Ord VCSBackend where
  compare (VCSB_Native s _ _) (VCSB_Native s' _ _) = compare s s'
  compare (VCSB_Native _ _ _) _ = LT
  compare VCSB_None VCSB_None = EQ
  compare VCSB_None _ = GT
nativeBackend :: MonadIO m => String -> PortNumber -> m VCSBackend
nativeBackend h p = do
  conn <- liftIO (connectTo h p)
  return (VCSB_Native ("curly-vc://"+h+":"+show p) conn (\(Client_VC io) -> io))
instance Show VCSBackend where
  show (VCSB_Native s _ _) = s
  show VCSB_None = "none"
instance Read VCSBackend where
  readsPrec _ = readsParser $ backend
    where backend = curlyBackend <+? fill VCSB_None (several "none")
          curlyBackend = do
            several "curly-vc://" <+? single '@'
            map (by thunk) $ liftA2 nativeBackend
              (many1' (noneOf ":") <&> \x -> if x=="_" then "127.0.0.1" else x)
              (option' 5402 (single ':' >> number))
              
curlyVCSBackend :: VCSBackend
curlyVCSBackend = fromMaybe (getDefaultVCS^.thunk) (matches Just readable (envVar "" "CURLY_VCS"))
  where getDefaultVCS = try (return VCSB_None) $ do
          lns <- map words . lines <$> readProcess "/usr/lib/curly/default-vcs" [] ""
          case lns of
            ([h,p]:_) -> nativeBackend h (fromInteger $ read p)
            _ -> return VCSB_None
            
curlyPublisher :: String
curlyPublisher = envVar "" "CURLY_PUBLISHER"

getVCSBranches :: MonadIO m => String -> m Branches
getVCSBranches name = do
  u <- lookup name <$> getKeyStore
  case (curlyVCSBackend,u) of
    (VCSB_Native _ st run,Just (_,pub,_,_,_)) -> liftIO $ do
      map (maybe zero unsafeExtractSigned) $ run $ vcLoad st (BranchesKey pub)
    _ -> return zero
