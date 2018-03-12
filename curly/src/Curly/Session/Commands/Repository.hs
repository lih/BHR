{-# LANGUAGE CPP, ExistentialQuantification, ViewPatterns, RecursiveDo #-}
module Curly.Session.Commands.Repository where

import Curly.Core
import Curly.Core.Library
import Curly.Core.Parser
import Curly.Core.Security
import Curly.Core.VCS
import Curly.Core.VCS.Diff
import Curly.Core.Documentation
import Curly.Session.Commands.Common
import Curly.UI
import Curly.UI.Options
import Definitive
import IO.Filesystem
import Language.Format
import System.FilePath (splitFileName)
import Data.IORef

repositoryCmd :: Interactive Command

repositoryDoc = unlines [
  "{section {title Manage Repository}",
  " {p A command to manage repositories and their contents}",
  " {title Usage}",
  ul [
    li "{em repository info} {p List all active repositories}",
    li "{em repository add <repository>} {p Add a repository to the list of active repositories}",
    li "{em repository browse (<library-id>|<search-pattern>)} {p Open a sub-session in the context of an external library}",
    li (fold ["{em repository list [<template> [<key-name> [<branch>]]]} ",
              "{p List the branches published by <key-name>}",
              "{ln If a branch name is specified, lists that branch's libraries instead.}"]),
    li (fold ["{em repository commit <branch> <modifier>...}",
              "{p Push a new commit on the given branch, by applying various modifiers in order}",
              "{p The <modifier>s can be either of the following: ",
              "  {ul {li {em -add <path>...} Adds the libraries under <path> to the branch}",
              "      {li {em -(keep|drop) (<library-id>|<search-pattern>|(maximum|minimum) <template> by <template>)} ",
              "          {ln Filters the branch according to a pattern.}}}}"]),
    li (fold ["{em repository branch <branch> ((fork|alias) <key-name> <source-branch>|rename <new-name>|delete)}",
              "{p Create a new branch that points to the same commit a another.}",
              "{p The 'alias' option creates an alias branch rather than a fork.",
              "    Alias branches will always be resolved to the latest commit on their source branch.}"]),
    li (fold ["{em repository get (source|library) <filename> (#<library-id>|<search-pattern>)}",
              " {p Retrieve a library or its source and saves it to a file}"]),
    li (fold ["{em repository checkout <source-prefix> (#<library-id>|<search-pattern>)}",
              "{p Reconstruct a working source tree for the given library}"])
    ],
  "}"]
  where li = format "{li.p %s}"
        ul l = format "{ul %s}" (intercalate " " l)
repositoryCmd = withDoc repositoryDoc $ False <$ do
  cmd <- expected "keyword, either 'commit', 'list' or 'get-source'" (nbhspace >> dirArg)
  u <- lookup curlyPublisher <$> getKeyStore
  conn <- liftIO (readIORef libraryVCS)
        
  let withKeys k = case u of
        Just (_,pub,Just priv,_,_) -> k pub priv
        _ -> serveStrLn (format "Error: the publisher %s doesn't have a private key" curlyPublisher) >> zero
      modifyBranches :: (Branches -> OpParser IO Branches) -> OpParser IO () 
      modifyBranches k = withKeys $ \pub priv -> do
        StampedBranches stamp bs <- getBranches conn pub
        x <- k bs
        bs' <- signValue priv (StampedBranches (1+stamp) x)
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
    "info" -> liftIOWarn $ do
      VCSB_Native repos _ _ <- readIORef libraryVCS
      for_ repos $ \r -> serveStrLn $ format "  * %s" r
    "commit" -> withMountain $ do
      guardWarn Sev_Error "Cannot commit without admin access" (?access >= Admin)
      let branchFilter = do
            filterP <- expected "'keep' or 'drop'" (fill id (several "-keep")
                                                    <+? fill not (several "-drop"))
            let libPred = (single '#' >> (dirArg >*> readable)) <&> \l -> warp ascList $ \ls -> [x | x@(l',_) <- ls, filterP (l==l')]
                tplAtom = docAtom <*= \x -> guard (has t'Join x)
                singlePred = do
                  tpl <- tplAtom <+? map snd packageSearch
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
            expected "filter predicate" (nbhspace >> (groupPred <+? libPred <+? singlePred))
            
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

    "add" -> do
      guardWarn Sev_Error "you must have admin access to add repositories" (?access>=Admin)
      r <- nbsp >> many' (noneOf "\n") >*> readable
      liftIOWarn $ modifyIORef libraryVCS (+ r)

    "list" -> do
      template <- option' (docTag' "call" [Pure "list-default"])
                  ((\case Pure p -> docTag' p []; x -> x)
                   <$> (nbhspace >> docAtom))
      mkeyid <- expected "key name" (option' Nothing (nbhspace >> Just<$>dirArg))
      branch <- option' Nothing (Just <$> (nbhspace >> dirArg))
      lookingAt (hspace >> (eol+eoi))
      case mkeyid of
        Nothing -> do
          liftIOWarn $ withStyle $ withPatterns $ do
            ls <- availableLibs
            for_ [(l,d) | (l,Just d) <- map (second (\d -> showTemplate ?terminal ?style ?patterns d template)) ls] $ \(l,d) -> do
              serveStrLn $ format "%s %s" (show l) d
        Just keyid -> do
          key <- lookup keyid <$> getKeyStore
          case (map (by l'2) key,branch) of
            (Nothing,_) -> serveStrLn $ format "Error: Unknown key %s" keyid
            (Just pub,Nothing) -> withMountain $ do
              StampedBranches _ m <- map (maybe (StampedBranches zero zero) unsafeExtractSigned) $ vcbLoad conn (BranchesKey pub)
              serveStrLn $ intercalate "\n" [format "%s: %s" n (show (Zesty c)) | (n,c) <- m^.ascList]
            (Just pub,Just b) -> withMountain $ withStyle $ withPatterns $ do
              bs <- maybe (return zero) (getCommit conn) =<< getBranch conn (Just (Left (pub,b)))
              forl_ (ascList.each) bs $ \(lid,m) -> do
                for_ (showTemplate ?terminal ?style ?patterns m template) $ \s -> 
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
      guardWarn Sev_Error "Cannot modify a branch without admin access" (?access >= Admin)
      branch <- expected "branch name" (nbhspace >> dirArg)
      let branchFork = do
            isLink <- fill False (several "fork") <+? fill True (several "alias")
            user <- expected "key id" (nbhspace >> dirArg)
            srcBranch <- expected "branch name" (nbhspace >> dirArg)
            map (lookup user) getKeyStore >>= \x -> case x of
              Nothing -> do serveStrLn $ format "Error: unknown user %s" user
                            zero
              Just (_,pub,_,_,_) -> modifyBranches $ \bs -> do
                if isLink then return (insert branch (Left (pub,srcBranch)) bs)
                  else do 
                  StampedBranches _ bs' <- getBranches conn pub
                  return (set (at branch) (bs'^.at srcBranch) bs)
          branchRen = do
            several "rename"
            newName <- nbhspace *> dirArg <* ack 
            modifyBranches $ return . (join $ \bs -> set (at newName) (bs^.at branch) . delete branch)
          branchDel = do
            several "delete" <* ack
            modifyBranches $ return . delete branch
            
      nbhspace >> (branchFork <+? branchRen <+? branchDel)

    "browse" -> do
      guardWarn Sev_Error "you must have almighty acces to browse new libraries" (?access>=Almighty)
      nbsp
      l <- libID
      ?subSession [(Nothing,Mount [] (Library l))]
  
    _ -> warn Sev_Error "Expected 'commit', 'list', 'get-library' or 'get-source'" >> zero
  where ack = lookingAt (hspace >> (eol+eoi))
        libID = (single '#' >> (dirArg >*> readable)) <+? searchID
        createFileDir f = createDirectoryIfMissing True (dropFileName f)

        ioParser m = liftIO (try (return Nothing) (Just<$>m)) >>= maybe zero return
            
        searchID = ((docAtom <*= guard . has t'Join) <+? (snd <$> packageSearch)) >>= \tpl -> do
          ls <- liftIO availableLibs
          case fold [convert (showDummyTemplate m tpl >> return l) | (l,m) <- ls] of
            [l] -> return l
            [] -> guardWarn Sev_Error (format "Error: couldn't find a library matching the search pattern '%s'" (showRawDoc tpl)) False >> zero
            _ -> guardWarn Sev_Error (format "Error: multiple libraries match the search pattern '%s'" (showRawDoc tpl)) False >> zero
