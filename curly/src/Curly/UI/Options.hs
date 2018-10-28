{-# LANGUAGE CPP, ScopedTypeVariables #-}
module Curly.UI.Options (
  -- * The global mode of operation
  CurlyPlex(..),mounts,targets,mountainCache,jitContext,newCurlyPlex,

  -- * Inputs and targets
  CurlyOpt(..),CurlyCondOpt(..),InputSource(..),Target(..),
  t'Mount,t'Target,t'Source,t'Help,t'setting,targetFilepaths,readServer,showOpts,

  -- * Per-target threaded configuration
  TargetParams,confServer,confPrelude,confBanner,confInstance,confThreads,defaultConf,getConf,withPrelude,

  -- * Misc
  curlyOpts,packageID,packageSearch,inputSource,curlyFileName,noCurlySuf,visible,symPath,showSymPath
  ) where

import Definitive
import Curly.Core
import Curly.Core.Library
import Curly.Core.Documentation
import Language.Syntax.CmdArgs
import Curly.System.Base (System)
import Curly.System 
import Control.Concurrent (ThreadId)
import Data.IORef (IORef,newIORef)

newtype CurlyCondOpt = CurlyCondOpt (DocParams -> CurlyOpt)
instance Show CurlyCondOpt where
  show (CurlyCondOpt f) = show (f zero)
instance Eq CurlyCondOpt where a == b = compare a b == EQ
instance Ord CurlyCondOpt where compare = comparing (\(CurlyCondOpt f) -> f zero)

data CurlyOpt = Mount [String] InputSource
              | Target Target
              | Flag String
              | FlagDescription String String
              | Conditional (Map String [String]) (Set String) CurlyCondOpt
              deriving (Eq,Ord,Show)
t'Mount :: Traversal' CurlyOpt ([String],InputSource)
t'Mount k (Mount s i) = k (s,i) <&> \(s',i') -> Mount s' i'
t'Mount _ x = pure x
t'Target :: Traversal' CurlyOpt Target
t'Target k (Target t) = Target<$>k t
t'Target _ x = pure x

data InputSource = Source [String] String String
                 | Resource String String
                 | Library LibraryID
                 | LibraryFile String
                 deriving (Eq,Ord)
instance Show InputSource where
  show (Source p s c) = "source"+showSub+" "+s+" "+c
    where showSub | empty p = ""
                  | otherwise = "["+intercalate "." p+"] "
  show (Resource p c) = "resource "+p+" "+c
  show (Library i) = "library "+show i
  show (LibraryFile f) = "library @"+f
instance FormatArg InputSource where argClass _ = 'I'

t'Source :: Traversal' InputSource ([String],String,String)
t'Source k (Source b s c) = k (b,s,c) <&> \(b',s',c') -> Source b' s' c'
t'Source _ x = pure x

type InstanceName = String
type HostName = String

data Target = Help | Version 
            | Interactive
            | Execute String
            | RunFile FilePath
            | Goody FilePath
            | SetPrelude String
            | AddPrelude String
            | SetBanner String
            | AddBanner String
            | SetServer (Maybe (InstanceName,HostName,PortNumber))
            | SetInstance InstanceName
            | ServeInstance | ListInstances
            | Echo String
            | Translate FilePath System [String]
            deriving (Eq,Ord)
instance Show Target where
  show Help = "help"
  show Version = "version"
  show Interactive = "interactive"
  show (Execute s) = "execute "+s
  show (RunFile s) = "run "+s
  show ServeInstance = "serve-instance"
  show ListInstances = "list-instances"
  show (SetPrelude p) = "prelude  "+p
  show (AddPrelude p) = "prelude+ "+p
  show (SetBanner b) = "banner  "+b
  show (AddBanner b) = "banner+ "+b
  show (SetServer Nothing) = "at local"
  show (SetServer (Just (i,h,p))) = format "at %s:%p/%s" h p i
  show (SetInstance i) = "instance "+i
  show (Echo s) = "echo "+s
  show (Translate f s p) = format "translate %s @ %s = %s" f (show s) (intercalate "." p) 
  show (Goody f) = "goody "+f
  
instance FormatArg Target where argClass _ = 'T'
t'Help :: Traversal' Target ()
t'Help k Help = Help <$ k ()
t'Help _ x = return x
t'setting :: Traversal' Target ()
t'setting k x@(SetInstance _) = x <$ k ()
t'setting k x@(SetPrelude _) = x <$ k ()
t'setting k x@(AddPrelude _) = x <$ k ()
t'setting k x@(SetServer _) = x <$ k ()
t'setting k x@(SetBanner _) = x <$ k ()
t'setting k x@(AddBanner _) = x <$ k ()
t'setting _ x = return x
targetFilepaths :: Traversal' Target String
targetFilepaths k (Translate n s p) = k n <&> \n' -> Translate n' s p
targetFilepaths k (RunFile f) | f/="-" = RunFile<$>k f
targetFilepaths _ t = return t

data TargetParams = TargetParams {
  _confServer :: Maybe (String,String,PortNumber),
  _confBanner :: String,
  _confPrelude :: [String],
  _confInstance :: String,
  _confThreads :: Maybe (ThreadId,ThreadId,IORef (Int,Map Int ThreadId))
  }
confServer :: Lens' TargetParams (Maybe (String,String,PortNumber))
confServer = lens _confServer (\x y -> x { _confServer = y })
confPrelude :: Lens' TargetParams [String]
confPrelude = lens _confPrelude (\x y -> x { _confPrelude = y })
confBanner :: Lens' TargetParams String
confBanner = lens _confBanner (\x y -> x { _confBanner = y })
confInstance :: Lens' TargetParams String
confInstance = lens _confInstance (\x y -> x { _confInstance = y })
confThreads :: Lens' TargetParams (Maybe (ThreadId,ThreadId,IORef (Int,Map Int ThreadId)))
confThreads = lens _confThreads (\x y -> x { _confThreads = y })

defaultBanner :: String
defaultBanner = unlines [
  "+---------------------------------------------------------------------------+",
  "|      __     __ _ __ ___   __  _  __                                       |",
  "|   __/  ` ,'_//// // o | / / | |/,'   Welcome to the Curly Shell !         |",
  "|  / /\\   / /_/ U //  ,' / /_ | ,'                                          |",
  "| |  \\/   |__/\\_,'/_/`_\\/___//_/       Enter 'help' if you need it          |",
  "| \\                           v"+VERSION_curly+drop (length VERSION_curly)
                                 "                                             |",
  "+---------------------------------------------------------------------------+"
  ]
defaultConf :: TargetParams
defaultConf = TargetParams zero defaultBanner zero "curly" zero
getConf :: (?targetParams :: TargetParams) => Lens' TargetParams a -> a
getConf l = ?targetParams ^. l
withPrelude :: (?targetParams :: TargetParams) => String -> String
withPrelude s = getConf confPrelude & \p -> intercalate "\n" (p+[s])

curlyOpts = [
  Option ['h'] ["help"] (NoArg (target Help)) "Display the help menu and some basic information (inhibits other flags)",
  Option ['v'] ["version"] (NoArg (target Version)) "Show the current Curly version",
  Option [] ["goody"] (ReqArg (target . Goody) "FILE") "Dump the contents of an installed data file. The 'list' files contains all available names",
  sepOpt "Inputs",
  Option ['M'] ["mount"] (ReqArg mkMount "PATH=MOUNT") "Mount an input source to a path in the default context",
  sepOpt "Outputs",
  Option ['t'] ["translate"] (ReqArg (target . mkTranslate) "FILE[@SYS][=PATH]") "Translate a Curly function for a system",
  sepOpt "Session Context",
  Option ['P'] ["prelude"] (ReqArg (target . SetPrelude) "COMMAND") "Set the prelude for the next targets",
  Option ['p'] ["prelude+"] (ReqArg (target . AddPrelude) "COMMAND") "Append the given command to the prelude",
  Option [] ["banner"]  (ReqArg (target . SetBanner) "BANNER") "Set the banner for the next targets",
  Option [] ["banner+"]  (ReqArg (target . AddBanner) "BANNER") "Add a line to the banner file for the next targets",
  Option [] ["instance"] (ReqArg (target . SetInstance) "INSTANCE") "Set the instance name for the next targets",
  Option [] ["at"] (ReqArg (target . SetServer . readServer) "[SERVER]/INSTANCE") "Select a server for the next targets",
  sepOpt "Running Sessions",
  Option ['i'] ["interactive"] (NoArg (target Interactive)) "Launch an interactive session",
  Option ['e'] ["execute"]     (ReqArg (target . Execute) "COMMAND") "Execute an interactive command",
  Option ['r'] ["run"]    (ReqArg (target . RunFile) "FILE") "Run interactive commands from the given file, or stdin if the file is -",
  sepOpt "Hosting Sessions",
  Option ['s'] ["serve-instance"] (NoArg (target ServeInstance)) "Launch an instance server for the current instance.",
  Option ['l'] ["list-instances"] (NoArg (target ListInstances)) "List all available instances on the selected server (the previous --at target)"
  ]
  where tryParse err p s = fromMaybe (error (err s)) (matches Just p s)
        mkMount = tryParse (format "Couldn't parse mount option '%s'") (inputSource "." <&> uncurry Mount)
        mkTranslate = tryParse (format "Coudn't parse translate option '%s' (expected FILE=MODULE:...:FUNCTION)") translate
        sepOpt dsc = Option [] [] (NoArg (target Help)) ("--- " + dsc + " ---")
        target t = Target t
          
        translate = do
          n <- visible "@=" <* hspace
          s <- option' hostSystem (single '@' >> hspace >> visible "="
                                  >>= \s -> maybe zero return (knownSystems^.at s))
          let splitPath ('.':t) = "":splitPath t
              splitPath ('-':t) = "":splitPath t
              splitPath (c:t)   = let ~(h:t') = splitPath t in (c:h):t'
              splitPath [] = [""]
          p <- option' (splitPath n) $ do
            between hspace hspace $ single '=' 
            symPath ""
          return (Translate n s p)

grid :: String -> [[String]] -> String
grid sep l = unlines $ map (intercalate sep . zipWith padTo lengths) l
  where padTo n s | length s>=n = s
                  | otherwise = s+take (n-length s) (repeat ' ')
        lengths = mk l
          where mk (h:t) = zipWith max (map length h+repeat 0) (mk t)
                mk [] = repeat 0

showOpts opts = grid "  " $ [
  [intercalate "," (map (\x -> ['-',x]) short),intercalate "," (map ("--"+) long)
  ,case arg of
     ReqArg _ a -> a
     _ | empty short && empty long -> ""
       | otherwise -> "(nothing)"
  ,desc]
  | Option short long arg desc <- opts]

readServer s = matches Just (
  liftA3 (\h p i -> (i,h,p))
  (option' "127.0.0.1" $ many1' (noneOf "/:"))
  (option' 25465 (single ':' >> number))
  (single '/' >> remaining)) s

visible :: (MonadParser s m p, ParseStream s,Monad m ,StreamChar s ~ Char) => [Char] -> p String
visible lim = many1' ((single '\\' >> token) <+? satisfy (not . \c -> c==' ' || c=='\t' || c=='\n' || c`elem`lim))

symPath :: (MonadParser s m p, ParseStream s,Monad m, StreamChar s ~ Char) => String -> p [String]
symPath lim = sepBy' (visible ('.':lim)) (single '.')
showSymPath :: [String] -> String
showSymPath p = intercalate "." (map (foldMap quote) p)
  where quote '.' = "\\."
        quote c = [c]

packageSearch :: (ParseStream s, Monad m, StreamChar s ~ Char, MonadParser s m p) => p ((String,Maybe String,Maybe String),Template)
packageSearch = do
  keyname <- optionMaybe' (visible ":" <* single ':')
  branch <- optionMaybe' (visible ":" <* single ':')
  name <- searchForward (several "-v" <+? eol <+? nbhspace <+? eoi)
  version <- optionMaybe' (several "-v" >> visible "")
  let eqtag ts v = docTag' "=" [docTag' "$" (map Pure ts),Pure v]
  return $ ((name,keyname,version),) $ docTag' "and"
    $ eqtag ["name"] name
    : convert (map (eqtag ["repository","branch-name"]) branch)
    + convert (map (eqtag ["repository","key-name"]) keyname)
    + convert (map (eqtag ["version"]) version)
  where searchForward p = search ""
          where search acc = (fill (reverse acc) $ lookingAt p) <+? (notLookingAt p >> token >>= \c -> search (c:acc))
 
inputSource base = do
  p <- symPath "="
  between hspace hspace (several "=")
  (p,) <$> (src <+? rsc <+? lib <+? search <+? blts)
  where sep = nbhspace <+? between hspace hspace (single ':')
        src = do
          like "source"
          sub <- option' [] (between (single '[') (single ']')
                             $ between hspace hspace
                             $ symPath "]")
          sep
          n <- visible ":"
          let defaultCache = fromMaybe (n+".cache") (noCurlySuf n <&> (+".cyl"))
          m <- option' defaultCache (sep >> visible "")
          return (Source sub (base</>n) (base</>m))
        rsc = do
          like "resource"
          n <- sep >> visible ":"
          m <- option' (n+".cache") (sep >> visible "")
          return (Resource (base</>n) (base</>m))
        search = like "package" >> sep >> do
          tpl <- (docAtom <*= guard . has t'Join) <+? (snd <$> packageSearch)
          return (Library $ packageID tpl^.thunk)
        lib = like "library" >> sep >> (fileLib <+? map Library readable)
          where fileLib = single '@' >> map LibraryFile (visible "")
        blts = Library (builtinsLib^.flID) <$ like "builtins"

packageID :: Template -> IO LibraryID
packageID tpl = do
  ls <- availableLibs
  case [l | (l,d) <- ls
          , nonempty (showDummyTemplate d tpl)] of
    [l] -> return l
    [] -> error $ format "No package found when search for '%s'" (showRawDoc tpl)
    _ -> error $ format "Multiple packages found matching '%s'. Please narrow your search parameters." (showRawDoc tpl)

data CurlyPlex = CurlyPlex {
  _mounts :: [([String],InputSource)],
  _targets :: [Target],
  _mountainCache :: IORef (Mountain,[Mountain -> IO ()]),
  _jitContext :: JITContext GlobalID
  }
mounts :: Lens' CurlyPlex [([String],InputSource)]
mounts = lens _mounts (\x y -> x { _mounts = y })
targets :: Lens' CurlyPlex [Target]
targets = lens _targets (\x y -> x { _targets = y })
mountainCache :: Lens' CurlyPlex (IORef (Mountain,[Mountain -> IO ()]))
mountainCache = lens _mountainCache (\x y -> x { _mountainCache = y })
jitContext :: Lens' CurlyPlex (JITContext GlobalID)
jitContext = lens _jitContext (\x y -> x { _jitContext = y })

curlyFileName s = fromMaybe (0 :: Int,"") $ matches Just axiom s
  where axiom = liftA2 (,) (option 0 num) tl 
        num = number <* satisfy (`elem`".-: ")
        tl = remaining

newCurlyPlex = CurlyPlex [] [] <$> newIORef zero <*> newJITContext
