module Curly.Session.Commands.VCS(vcsCmd) where

import Curly.Core
import Curly.Core.Library
import Curly.Core.Parser
import Curly.Core.Security
import Curly.Core.VCS
import Curly.Core.VCS.Diff
import Curly.Session.Commands.Common
import Curly.UI
import Definitive
import IO.Filesystem
import Language.Format
import System.FilePath (splitFileName)
import Data.IORef

vcsCmd :: Interactive Command

vcsDoc = unlines [
  "{section {title Version Control}",
  " {p A command to handle version-control related tasks}",
  " {title Usage}",
  ul [ 
    li (fold ["{em vcs list <key-name> [<branch> [<template>]]}: ",
              "{p List the branches published by <key-name>}",
              "{ln+sub+sub If a branch name is specified, lists that branch's libraries instead.}"]),
    li (fold ["{em vcs commit <branch> <modifier>...}:",
              "{p Pushes a new commit on the given branch, by applying various modifiers in order}",
              "{p The <modifier>s can be either of the following: ",
              "  {ul {li {em -add <path>...} Adds the libraries under <path> to the branch}",
              "      {li {em -(keep|drop) (<library-id>|<search-pattern>|(maximum|minimum) <template> by <template>)} ",
              "          {ln Filters the branch according to a pattern.}}}}"]),
    li (fold ["{em vcs branch <branch> (fork|alias) <key-name> <source-branch>}",
              "{p Creates a new branch that points to the same commit a another.}",
              "{p The 'alias' option creates an alias branch rather than a fork.",
              "    Alias branches will always be resolved to the latest commit on their source branch.}"]),
    li (fold ["{em vcs get (source|library) <filename> (<library-id>|<search-pattern>)}:",
              " Retrieves a library or its source and saves it to a file"]),
    li (fold ["{em vcs checkout <source-prefix> (<library-id>|<search-pattern>)}:",
              "Reconstructs a working source tree for the given library"])
    ],
  "}"]
  where li = format "{li.p %s}"
        ul l = format "{ul %s}" (intercalate " " l)
vcsCmd = withDoc vcsDoc $ False <$ do
  cmd <- expected "keyword, either 'commit', 'list' or 'get-source'" (nbhspace >> dirArg)
  u <- lookup curlyPublisher <$> getKeyStore
  conn <- liftIO (readIORef libraryVCS)
        
  let withKeys k = case u of
        Just (_,pub,Just priv,_,_) -> k pub priv
        _ -> serveStrLn (format "Error: the publisher %s doesn't have a private key" curlyPublisher) >> zero
      modifyBranches :: (Branches -> OpParser IO Branches) -> OpParser IO () 
      modifyBranches k = withKeys $ \pub priv -> do
        bs <- getBranches conn pub
        x <- k bs
        bs' <- signValue priv x
        vcbStore conn (BranchesKey pub) bs'
      getSource file lid = do
        x <- vcbLoad conn (AdditionalKey lid "source")
        case x of
          Just s -> liftIO $ do
            createFileDir file
            writeBytes file (snd (unsafeExtractSigned s))
          Nothing -> serveStrLn $ format "Warning: cannot find source for the library %s" (show lid)
      getLibrary file lid = do
        x <- vcbLoad conn (LibraryKey lid)
        case x of
          Just s -> when (isLibData lid s) $ liftIO $ do
            createFileDir file
            writeBytes file s
            modifyPermissions file (set (each.executePerm) True)
          Nothing -> serveStrLn $ format "Warning: the library %s doesn't seem to exist" (show lid)
      checkout pref lid = do
        serveStrLn $ format "Checking out %s.cy from library %s" pref (show lid)
        getSource (pref+".cy") lid
        getLibrary (pref+".cyl") lid
        ctx <- liftIO $ fromMaybe zero . by (metadata.at "context") <$> readFormat (pref+".cyl")
        let checkoutMod suf (Pure l) = let l' = read l in
              if l' `elem` map (by flID) builtinLibs
              then pure [(lid,suf,l',Nothing)]
              else ((lid,suf,l',Just pref):) <$> checkout (pref+foldMap ("/"+) suf) l'
            checkoutMod suf (Join m) = do
              map fold $ for (m^.ascList) $ \(d,m') -> do
                checkoutMod (suf+[d]) m'
        checkoutMod [] ctx
        
  case cmd of
    "commit" -> withMountain $ do
      guardWarn Sev_Error "Cannot commit without admin access" (?access >= Admin)
      let branchFilter = do
            filterP <- expected "'keep' or 'drop'" (fill id (several "-keep")
                                                    <+? fill not (several "-drop"))
            let libPred = (dirArg >*> readable) <&> \l -> warp ascList $ \ls -> [x | x@(l',_) <- ls, filterP (l==l')]
                tplAtom = docAtom <*= \x -> guard (has t'Join x)
                singlePred = do
                  tpl <- tplAtom
                  return $ warp ascList $ \ls -> [x | x@(_,m) <- ls, filterP (nonempty (showDummyTemplate m tpl))]
                groupPred = do
                  op <- fill "<=" (several "minimum") <+? fill ">=" (several "maximum")
                  cmptpl <- nbhspace >> docAtom
                  nbhspace >> several "by"
                  gtpl <- nbhspace >> tplAtom
                  let minTpl m1 m2 | nonempty (do v <- showDummyTemplate (snd m1) cmptpl
                                                  showDummyTemplate (snd m2) (Join (DocTag op [] [Pure v,cmptpl]))) = m1
                                   | otherwise = m2
                  return $ \ls -> let groups = c'map $ composing (\x@(_,m) -> mat (showDummyTemplate m gtpl) %~ (x:)) (ls^.ascList) zero
                                  in fromAList $ mlookup Nothing groups + fold [select (filterP . (fst (foldl1' minTpl l)==) . fst) l
                                                                               | (Just _,l) <- groups^.ascList]
            expected "filter predicate" (nbhspace >> (libPred <+? singlePred <+? groupPred))
            
          branchAdd = do
            several "-add"
            path <- nbhspace >> absPath ""
            let libs = ?mountain ^?? atMs path.traverse
      
            for_ libs $ \lib -> do 
              serveStrLn $ format "Committing library %s" (show (lib^.flID))
              vcbStore conn (LibraryKey (lib^.flID)) (lib^.flBytes)
              for_ (lib^.flSource) $ \s -> do
                serveStrLn $ format "Committing source for library %s" (show (lib^.flID))
                withKeys $ \_ priv -> do
                  s' <- signValue priv ("source",stringBytes s)
                  vcbStore conn (AdditionalKey (lib^.flID) "source") s'
            
            return $ composing (uncurry insert) [(fl^.flID,fl^.flLibrary.metadata) | fl <- libs]

      branch <- expected "branch name" (nbhspace >> dirArg)
      pred <- compose <$> many1' (nbhspace >> muteOnSuccess (branchFilter <+? branchAdd))
      lookingAt (hspace >> (eol+eoi))
      serveStrLn $ format "Writing new commit on the '%s' branch" branch
      modifyBranches $ \bs -> do
        mh <- ioParser (getBranch conn (lookup branch bs))
        index <- ioParser (maybe (return zero) (getCommit conn) mh)
        let index' = pred index
            dff = diff index index'
            c = Compressed (dff,mh)
            commid = commitHash c
        vcbStore conn (CommitKey commid) c
        return (insert branch (Right commid) bs)
          
    "list" -> do
      keyid <- expected "key name" (nbhspace >> dirArg)
      branch <- option' Nothing (Just <$> (nbhspace >> dirArg))
      template <- option' Nothing (Just <$> docLine "template" [])
      lookingAt (hspace >> (eol+eoi))
      key <- lookup keyid <$> getKeyStore
      case map (by l'2) key of
        Nothing -> serveStrLn $ format "Error: Unknown key %s" keyid
        Just pub -> withMountain $ case branch of
          Nothing -> do
            m <- map (maybe zero unsafeExtractSigned) $ vcbLoad conn (BranchesKey pub)
            serveStrLn $ intercalate "\n" [format "%s: %s" n (show (Zesty c)) | (n,c) <- m^.ascList]
          Just b -> withStyle $ withPatterns $ do
            bs <- maybe (return zero) (getCommit conn) =<< getBranch conn (Just (Left (pub,b)))
            forl_ (ascList.each) bs $ \(lid,m) -> do
              for_ (maybe (Just $ show m) (showTemplate ?terminal ?style ?patterns m) template) $ \s -> 
                serveStrLn $ format "%s %s" (show lid) s

    "get" -> do
      guardWarn Sev_Error "You must have almighty access to retrieve arbitrary files" (?access >= Almighty)
      getLib <- expected "'source' or 'library'" (nbhspace >> (fill True (several "library")
                                                            <+? fill False (several "source")))
      file <- expected "file name" (nbhspace >> dirArg)
      lid <- expected "library ID" (nbhspace >> libID)
      (if getLib then getLibrary else getSource) file lid
                  
    "checkout" -> do
      guardWarn Sev_Error "Checkouts can only be performed with almighty access" (?access >= Almighty)
      (root,name) <- splitFileName <$> expected "file prefix" (nbhspace >> dirArg)
      let pref = root+name
      lid <- expected "library ID" (nbhspace >> libID)
      ls <- checkout pref lid
      liftIO $ do
        writeString (root+".curly") $ unlines [
          "#!/usr/bin/env curly",
          intercalate "\n" [format "mount deps.%s%s = %s"
                            (show l) (foldMap ("."+) suf)
                            $ c'string $ case x of
                              Just pref -> format "source[deps.%s] %s.cy"
                                           (show l') (drop (length root) pref+foldMap ("/"+) suf)
                              Nothing -> format "library %s" (show l')
                           | (l,suf,l',x) <- ls],
          format "mount root = source[deps.%s] %s.cy" (show lid) name,
          "+default - interactive"
          ]
        modifyPermissions (root+".curly") (set (each.executePerm) True)

    "branch" -> do
      guardWarn Sev_Error "Cannot modify a branch without almighty access" (?access >= Almighty)
      branch <- expected "branch name" (nbhspace >> dirArg)
      let branchFork = do
            isLink <- nbhspace >> (fill False (several "fork") <+? fill True (several "alias"))
            user <- expected "key id" (nbhspace >> dirArg)
            srcBranch <- expected "branch name" (nbhspace >> dirArg)
            map (lookup user) getKeyStore >>= \x -> case x of
              Nothing -> do serveStrLn $ format "Error: unknown user %s" user
                            zero
              Just (_,pub,_,_,_) -> modifyBranches $ \bs -> do
                if isLink then return (insert branch (Left (pub,srcBranch)) bs)
                  else do 
                  bs' <- getBranches conn pub
                  return (set (at branch) (bs'^.at srcBranch) bs)
            
      branchFork
        
    _ -> warn Sev_Error "Expected 'commit', 'list', 'get-library' or 'get-source'" >> zero
  where libID = searchID <+? (dirArg >*> readable)
        createFileDir f = createDirectoryIfMissing True (dropFileName f)

        ioParser m = liftIO (try (return Nothing) (Just<$>m)) >>= maybe zero return
            
        searchID = docAtom >>= \d -> do
          guard (has t'Join d)
          ls <- liftIO availableLibs
          case fold [showDummyTemplate m d >> return l | (l,m) <- ls] of
            Just l -> return l
            Nothing -> guardWarn Sev_Error (format "Error: no library matches the search pattern %s" (show d)) False >> zero
