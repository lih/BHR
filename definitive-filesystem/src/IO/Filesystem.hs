{-# LANGUAGE RankNTypes, ImplicitParams, CPP #-}
module IO.Filesystem (
  -- * Exported modules and functions
  module System.FilePath,module Definitive,
  createDirectoryIfMissing,(</>),followSymlink,
  
  -- * The File interface
  FileAttrs,lastMod,relPath,
  File(..),DirEntry(..),
  getFile,resource,
  
  workingDirectory,
  Location(..),
  pathTo,getConfig,

  -- ** A useful monad for manipulating the filesystem as a state
  FS(..),Filesystem,file,
  
  -- ** Status and PermMask
  modTime,PermMask,FilePermissions,ownerPerms,groupPerms,otherPerms,readPerm,writePerm,executePerm,
  runPermissionState,modifyPermissions,getPermissions,
  
  -- ** Useful Lenses
  contents,fileAttrs,children,child,descendant,subEntry,anyEntry,entryName,entryFile,
  named,withExtension,
  fileName,entry,text,bytes
  ) where

import Definitive
import Control.DeepSeq
import IO.Time
import System.Directory  hiding (getPermissions)
import System.FilePath (FilePath,dropFileName,takeFileName)
import System.IO.Unsafe
import System.Posix.Process (getProcessID)
import System.Posix.Files 
import System.Posix.Types (FileMode)
import Data.Time.Clock.POSIX
import qualified Prelude as P

type PermMask = (Bool,Bool,Bool)
data FilePermissions = FilePermissions { _ownerPerms,_groupPerms,_otherPerms :: PermMask }
                     deriving Show
instance Compound PermMask PermMask FilePermissions FilePermissions where
  each k (FilePermissions o g a) = liftA3 FilePermissions (k o) (k g) (k a)
ownerPerms,groupPerms,otherPerms :: Lens' FilePermissions PermMask
ownerPerms = lens _ownerPerms (\x y -> x { _ownerPerms = y })
groupPerms = lens _groupPerms (\x y -> x { _groupPerms = y })
otherPerms = lens _otherPerms (\x y -> x { _otherPerms = y })

readPerm,writePerm,executePerm :: Lens' PermMask Bool
readPerm = l'1 ; writePerm = l'2 ; executePerm = l'3

modePerms :: Iso' FileMode FilePermissions
modePerms = iso fromMode (\(FilePermissions (ord,ow,ox) (gr,gw,gx) (ar,aw,ax)) ->
                            (ord*^ownerReadMode)+^(ow*^ownerWriteMode)+^(ox*^ownerExecuteMode)+^
                            (gr*^groupReadMode)+^(gw*^groupWriteMode)+^(gx*^groupExecuteMode)+^
                            (ar*^otherReadMode)+^(aw*^otherWriteMode)+^(ax*^otherExecuteMode))
  where b *^ m = if b then m else nullFileMode
        (+^) = unionFileModes
        fromMode m = FilePermissions
                     (isSet ownerReadMode,isSet ownerWriteMode,isSet ownerExecuteMode) 
                     (isSet groupReadMode,isSet groupWriteMode,isSet groupExecuteMode)
                     (isSet otherReadMode,isSet otherWriteMode,isSet otherExecuteMode)
          where isSet sub = intersectFileModes m sub /= nullFileMode
        
runPermissionState :: FilePath -> State FilePermissions a -> IO a
runPermissionState f m = do
  e <- fileExist f
  mode <- if e then fileMode <$> getFileStatus f
          else return stdFileMode
  unless e (writeBytes f zero)
  let (m',a) = yb state m (mode^.modePerms)
  setFileMode f (m'^..modePerms)
  return a
modifyPermissions :: FilePath -> (FilePermissions -> FilePermissions) -> IO () 
modifyPermissions f m = runPermissionState f (modify m)
getPermissions :: FilePath -> IO FilePermissions
getPermissions f = try (return (nullFileMode^.modePerms)) (by modePerms . fileMode <$> getFileStatus f)

data FileAttrs = FileAttrs {
  _relPath :: FilePath,
  _lastMod :: TimeVal Seconds
  }
relPath :: Lens' FileAttrs FilePath
relPath = lens _relPath (\x y -> x { _relPath = y })
lastMod :: Lens' FileAttrs (TimeVal Seconds)
lastMod = lens _lastMod (\x y -> x { _lastMod = y })

data File = File FileAttrs (Maybe String) (Maybe Bytes)
          | Directory (Map String File) 
instance Show File where
  show (File p _ _) = "File "+(p^.relPath)
  show (Directory d) = show d
instance Semigroup File where
  Directory d + Directory d' = Directory ((d*d')+(d+d'))
  a + _ = a
instance Monoid File where zero = File (FileAttrs "" minBound) zero zero

data DirEntry = DirEntry FilePath File
              deriving Show
instance Lens1 String String DirEntry DirEntry where
  l'1 = from i'DirEntry.l'1
instance Lens2 File File DirEntry DirEntry where
  l'2 = from i'DirEntry.l'2
fileName :: Lens' DirEntry String
fileName = l'1
entry :: Lens' DirEntry File
entry = l'2

il :: IO a -> IO a
il = unsafeInterleaveIO
getFile :: FilePath -> IO File
getFile root = getFile' ""
  where getFile' rel = let path = root+rel in do
          d <- doesDirectoryExist path
          if d then do
            files <- unsafeInterleaveIO (getDirectoryContents path)
            return $ Directory $ fromAList [
              (name,unsafePerformIO (getFile' (rel</>name)))
              | name <- files, not (name`elem`[".",".."])]
            else File<$>(FileAttrs rel<$>il (modTime path))
                 <*>il (optional $ force<$>P.readFile path)
                 <*>il (optional $ readBytes path)
         
i'File :: ((FileAttrs,(Maybe String,Maybe Bytes)):+:Map String File) :<->: File
i'File = iso f' f
  where f (File p x y) = Left (p,(x,y))
        f (Directory d) = Right d
        f' = (\(x,(y,z)) -> File x y z) <|> Directory
i'DirEntry :: (FilePath,File) :<->: DirEntry
i'DirEntry = iso (uncurry DirEntry) (\ ~(DirEntry p f) -> (p,f))
contents :: Traversal' File (Maybe String,Maybe Bytes)
contents = from i'File.t'1.l'2
children :: Traversal' File (Map String File)
children = from i'File.t'2
child :: Traversal' File File
child = children.traverse
descendant :: Fold' File File
descendant = id .+ child.descendant
subEntry :: Traversal' DirEntry DirEntry
subEntry = entryFile.children.keyed.traverse.i'DirEntry
anyEntry :: Fold' DirEntry DirEntry
anyEntry = id .+ subEntry.anyEntry
entryName :: Lens' DirEntry String
entryName = from i'DirEntry.l'1
entryFile :: Lens' DirEntry File
entryFile = from i'DirEntry.l'2
text :: Traversal' File String
text = contents.lens fst (const (,zero)).folded
bytes :: Traversal' File Bytes
bytes = contents.l'2.folded
fileAttrs :: Traversal' File FileAttrs
fileAttrs = from i'File.t'1.l'1

named :: (String -> Bool) -> Traversal' DirEntry DirEntry
named p = sat (\(DirEntry name _) -> p name)
withExtension :: String -> Traversal' DirEntry DirEntry
withExtension e = named (\s -> drop (length s-(length e+1)) s==('.':e))

-- |The working directory, as a DirEntry
workingDirectory :: IO DirEntry
workingDirectory = DirEntry "." <$> (getFile =<< getCurrentDirectory)

modTime :: FilePath -> IO (TimeVal Seconds)
modTime p = try (return minBound) (getModificationTime p <&> pure . realToFrac . utcTimeToPOSIXSeconds)
followSymlink :: FilePath -> IO FilePath
followSymlink s = map (init (dropFileName s) </>) (readSymbolicLink s)

data Location = Here | Cache | Owner | Shared

pathTo :: ( ?programName :: FilePath ) => Location -> FilePath
pathTo Here = getCurrentDirectory^.thunk
pathTo Cache = (getTemporaryDirectory^.thunk) </> ?programName + "-" + show (getProcessID^.thunk) 
pathTo Owner = getHomeDirectory^.thunk </> "." + ?programName

#if MIN_VERSION_directory(1,2,3)
pathTo Shared = getXdgDirectory XdgData ?programName^.thunk
#else
pathTo Shared = "/usr/share" </> ?programName
#endif

getConfig :: ( ?programName :: FilePath ) => IO File
getConfig = sum<$>sequence [getFile (pathTo d) | d <- [Owner,Shared]]

resource :: (?programName :: FilePath) => FilePath -> FilePath
resource = (pathTo Shared </>)

instance NFData File where
  rnf (File _ _ (Just b)) = rnf b
  rnf (File _ (Just a) _) = rnf a
  rnf (Directory d) = rnf d
  rnf _ = ()

{-|
The FS monad is a wrapper around the IO monad that provides a
MonadState instance for interacting with the filesystem through the
Filesystem type.

Thus, you may use lenses to access the representation of files as
though they were variables, like so :

> runFS $ (file "x.bmp".bytes.serial.from bmp) ^>= \r ->
>     file "foo".bytes.serial.from jpg =- r

-}
newtype FS a = FS { runFS :: IO a }
               deriving (Functor,Unit,SemiApplicative,Applicative,Monad,MonadFix)
instance MonadState Filesystem FS where
  get = FS (return zero)
  put (Filesystem fs) = FS $ fs`deepseq`for_ (fs^.keyed) $ \(k,f) -> remove k >> putFile k f
    where putFile k (File _ _ (Just b)) = writeBytes k b
          putFile k (File _ (Just s) _) = P.writeFile k s
          putFile _ (File _ _ _) = unit
          putFile k (Directory d) = for_ (d^.keyed) $ \(k',f) -> putFile (k</>k') f
          remove f = doesDirectoryExist f >>= \x -> case x of
            True -> getDirectoryContents f >>= \c -> traverse_ (remove . (f+)) (c'list $ refuse (`elem`[".",".."]) c)
                                                     >> removeDirectory f
            False -> try unit (removeFile f)
  
newtype Filesystem = Filesystem (Map String File)
                   deriving (Semigroup,Monoid)
instance DataMap Filesystem String File where
  at k = lens f g
    where f (Filesystem m) = Just $ fromMaybe (getFile k^.thunk) $ m^.at k
          g (Filesystem m) x = Filesystem (insert k (fold x) m)

file :: String -> Lens' Filesystem File
file f = at f.folded

infixr 5 </>
(</>) :: FilePath -> FilePath -> FilePath
"." </> f = f
f </> "" = f
_ </> f@('/':_) = f
f </> ('.':'.':'/':t) | takeFileName f /= ".." = (case dropFileName f of "./" -> ""; x -> x) + t
                      | otherwise = f+"/../"+t
f </> f' = f+"/"+f'
