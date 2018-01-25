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

vcsCmd :: Interactive Command

vcsDoc = unlines [
  "{section {title Version Control}",
  " {p A command to handle version-control related tasks}",
  " {title Usage}",
  ul [
    li "{em vcs commit <branch> <path>...}: Commits all libraries under the given path",
    li (fold ["{em vcs get source <filename> (<library-id>|<search-pattern>)}:",
              " Retrieves a library's source and saves it to a file"]),
    li "{em vcs get library <filename> (<library-id>|<search-pattern>)}: Retrieves a library and saves it to a file",
    li (fold ["{em vcs checkout <source-prefix> (<library-id>|<search-pattern>)}:",
              "Reconstructs a working source tree for the given library"]),
    li (fold ["{em vcs branch <branch> (fork|link) <key-name> <other-branch>} Forks another branch.",
              "{ln+sub+sub If the fork type is 'link', the new branch will be an alias to the old one.}"]),
    li (fold ["{em vcs branch <branch> (keep|drop) (<library-id>|<search-pattern>|(maximum|minimum) <template> by <template>)}",
              "{ln+sub+sub Trims a branch by applying the given filter.}",
              "{ln+sub+sub Example: {em vcs branch my-branch keep maximum \\{$ version\\} by \\{$ name\\}}}"]),
    li (fold ["{em vcs list <key-name> [<branch> [<template>]]}: ",
              "List the branches published by <key-name>",
              "{ln+sub+sub If a branch name is specified, lists that branch's libraries instead.}"])
    ],
  "}"]
  where li = format "{li %s}"
        ul l = format "{ul %s}" (intercalate " " l)
vcsCmd = withDoc vcsDoc $ False <$ do
  cmd <- expected "keyword, either 'commit', 'list' or 'get-source'" (nbhspace >> dirArg)
  u <- lookup curlyPublisher <$> getKeyStore
  let withKeys k = case u of
        Just (_,pub,Just priv,_,_) -> k pub priv
        _ -> serveStrLn (format "Error: the publisher %s doesn't have a private key" curlyPublisher) >> zero
      modifyBranches :: (Branches -> OpParser IO Branches) -> OpParser IO () 
      modifyBranches k = withKeys $ \pub priv -> do
        bs <- getBranches pub
        x <- k bs
        bs' <- signValue priv x
        vcbStore conn (BranchesKey pub) bs'
      getBranches pub = maybe zero unsafeExtractSigned <$> vcbLoad conn (BranchesKey pub)
      deepBranch' Nothing = return Nothing
      deepBranch' (Just (Right h)) = return (Just h)
      deepBranch' (Just (Left (pub,b))) = deepBranch b pub
      deepBranch b pub = do
        bs <- getBranches pub
        deepBranch' (lookup b bs)
        
  case cmd of
    "commit" -> withMountain $ do
      guardWarn "Cannot commit without admin access" (?access >= Admin)
      branch <- expected "branch name" (nbhspace >> dirArg)
      pathtail <- many' (nbhspace >> dirArg) <* hspace <* lookingAt (eol+eoi)
      path <- getSession wd <&> (`subPath`pathtail)
      let libs = ?mountain ^?? atMs path.traverse

      for_ libs $ \lib -> do 
        serveStrLn $ format "Committing library %s" (show (lib^.flID))
        vcbStore conn (LibraryKey (lib^.flID)) (lib^.flBytes)
        for_ (lib^.flSource) $ \s -> do
          serveStrLn $ format "Committing source for library %s" (show (lib^.flID))
          withKeys $ \_ priv -> do
            s' <- signValue priv ("source",stringBytes s)
            vcbStore conn (AdditionalKey (lib^.flID) "source") s'

      serveStrLn $ format "Committing new libraries to the '%s' branch" branch
      modifyBranches $ \bs -> do
        mh <- deepBranch' (lookup branch bs)
        let c = Compressed (Patch [] [(fl^.flID,fl^.flLibrary.metadata) | fl <- libs]
                           ,mh)
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
            serveStrLn $ intercalate " " (keys m)
          Just b -> do
            bs <- getAll b =<< deepBranch b pub
            forl_ (ascList.each) bs $ \(lid,m) -> do
              for_ (maybe (Just $ show m) (showTemplate m) template) $ \s -> 
                serveStrLn $ format "%s %s" (show lid) s

    "get" -> do
      guardWarn "You must have almighty access to retrieve arbitrary files" (?access >= Almighty)
      getLib <- expected "'source' or 'library'" (nbhspace >> (fill True (several "library")
                                                            <+? fill False (several "source")))
      file <- expected "file name" (nbhspace >> dirArg)
      lid <- expected "library ID" (nbhspace >> libID)
      (if getLib then getLibrary else getSource) file lid
                  
    "checkout" -> do
      guardWarn "Checkouts can only be performed with almighty access" (?access >= Almighty)
      (root,name) <- splitFileName <$> expected "file prefix" (nbhspace >> dirArg)
      let pref = root+name
      lid <- expected "library ID" (nbhspace >> libID)
      ls <- checkout pref lid
      liftIO $ do
        writeString (root+name+".cyx") $ unlines [
          "#!/usr/bin/env curly",
          intercalate "\n" [format "mount deps %s%s = %s"
                            (show l) (foldMap (" "+) suf)
                            $ c'string $ case x of
                              Just pref -> format "source[deps %s] %s.cy"
                                           (show l') (drop (length root) pref+foldMap ("/"+) suf)
                              Nothing -> format "library %s" (show l')
                           | (l,suf,l',x) <- ls],
          format "mount root = source[deps %s] %s.cy" (show lid) name,
          "+default - interactive"
          ]
        modifyPermissions (pref+".cyx") (set (each.executePerm) True)

    "branch" -> do
      guardWarn "Cannot modify a branch without almighty access" (?access >= Almighty)
      branch <- expected "branch name" (nbhspace >> dirArg)
      let branchFilter = do
            filterP <- expected "'keep' or 'drop'" (nbhspace >> (fill id (several "keep")
                                                             <+? fill not (several "drop")))
            let libPred = (dirArg >*> readable) <&> \l ls -> [x | x@(l',_) <- ls, filterP (l==l')]
                tplAtom = docAtom <*= \x -> guard (has t'Join x)
                singlePred = do
                  tpl <- tplAtom
                  return $ \ls -> [x | x@(_,m) <- ls, filterP (nonempty (showTemplate m tpl))]
                groupPred = do
                  op <- fill "<=" (several "minimum") <+? fill ">=" (several "maximum")
                  cmptpl <- nbhspace >> docAtom
                  nbhspace >> several "by"
                  gtpl <- nbhspace >> tplAtom
                  let minTpl m1 m2 | nonempty (do v <- showTemplate (snd m1) cmptpl
                                                  showTemplate (snd m2) (Join (DocTag op [] [Pure v,cmptpl]))) = m1
                                   | otherwise = m2
                  return $ \ls -> let groups = c'map $ composing (\x@(_,m) -> mat (showTemplate m gtpl) %~ (x:)) ls zero
                                  in mlookup Nothing groups + fold [select (filterP . (fst (foldl1' minTpl l)==) . fst) l
                                                                   | (Just _,l) <- groups^.ascList]
            pred <- expected "filter predicate" (nbhspace >> (libPred <+? singlePred <+? groupPred))
            lookingAt (hspace >> (eol+eoi))
            modifyBranches $ \bs -> do
              mh <- deepBranch' (lookup branch bs)
              index <- getAll branch mh
              let index' = warp ascList pred index
                  dff = diff index index'
                  c = Compressed (dff,mh)
                  commid = commitHash c
              vcbStore conn (CommitKey commid) c
              return (insert branch (Right commid) bs)
          branchFork = do
            isLink <- nbhspace >> (fill False (several "fork") <+? fill True (several "link"))
            user <- expected "key id" (nbhspace >> dirArg)
            srcBranch <- expected "branch name" (nbhspace >> dirArg)
            map (lookup user) getKeyStore >>= \x -> case x of
              Nothing -> do serveStrLn $ format "Error: unknown user %s" user
                            zero
              Just (_,pub,_,_,_) -> modifyBranches $ \bs -> do
                if isLink then return (insert branch (Left (pub,srcBranch)) bs)
                  else do
                  bs' <- getBranches pub
                  return (bs & set (at branch) (bs'^.at srcBranch))
            
      branchFork <+? branchFilter
        
    _ -> guardWarn "Expected 'commit', 'list', 'get-library' or 'get-source'" False
  where conn = curlyVCSBackend
        libID = searchID <+? (dirArg >*> readable)
        createFileDir f = createDirectoryIfMissing True (dropFileName f)
        getSource file lid = do
          x <- vcbLoad conn (AdditionalKey lid "source")
          case x of
            Just s -> liftIO $ do
              createFileDir file
              writeBytes file (snd (unsafeExtractSigned s))
            Nothing -> serveStrLn $ format "Error: the source for library %s doesn't seem to exist" (show lid)
        getLibrary file lid = do
          x <- vcbLoad conn (LibraryKey lid)
          case x of
            Just s -> when (isLibData lid s) $ liftIO $ do
              createFileDir file
              writeBytes file s
              modifyPermissions file (set (each.executePerm) True)
            Nothing -> serveStrLn $ format "Error: the library %s doesn't seem to exist" (show lid)
        checkout pref lid = do
          serveStrLn $ format "Checking out %s.cy from library %s" pref (show lid)
          getSource (pref+".cy") lid
          getLibrary (pref+".cyl") lid
          ctx <- liftIO $ fromMaybe zero . by (metadata.at "context") <$> readFormat (pref+".cyl")
          let checkoutMod suf (Pure l) = let l' = read l in
                if l' /= builtinsLib^.flID 
                then ((lid,suf,l',Just pref):) <$> checkout (pref+foldMap ("/"+) suf) l'
                else pure [(lid,suf,l',Nothing)]
              checkoutMod suf (Join m) = do
                map fold $ for (m^.ascList) $ \(d,m') -> do
                  checkoutMod (suf+[d]) m'
          checkoutMod [] ctx
  
        getAll b (Just c) = cachedCommit c $ do
          comm <- vcbLoad conn (CommitKey c)
          case comm of
            Just (Compressed (p,mh)) -> patch p <$> getAll b mh
            Nothing -> do serveStrLn (format "Couldn't reconstruct the commit history for branch %s" b)
                          zero
        getAll _ Nothing = return zero
        cachedCommit c def = do
          let commitFile = cacheFileName curlyCommitDir (show (Zesty c)) "index"
          x <- liftIO $ try (return Nothing) (map (Just . unCompressed) $ readFormat commitFile)
          maybe (do liftIO $ createFileDirectory commitFile
                    def <*= liftIO . writeSerial commitFile . Compressed) return x
          
        searchID = docAtom >>= \d -> do
          guard (has t'Join d)
          ls <- liftIO availableLibs
          case fold [showTemplate m d >> return l | (l,m) <- ls] of
            Just l -> return l
            Nothing -> guardWarn (format "Error: no library matches the search pattern %s" (show d)) False >> zero
