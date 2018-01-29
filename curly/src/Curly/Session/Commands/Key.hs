{-# LANGUAGE CPP, ExistentialQuantification, ViewPatterns, RecursiveDo #-}
module Curly.Session.Commands.Key where

import Curly.Core
import Curly.Core.Security
import Curly.Core.Library
import Curly.UI.Options hiding (nbsp,spc)
import Curly.Core.Parser
import Language.Format hiding (space)
import Curly.Session.Commands.Common
import Curly.UI

keyCmd :: Interactive Command

keyDoc = unlines [
  "{section {title Manage Keystore}",
  "  {p Queries or modifies the keystore.}",
  "  {title Keystore Commands:}",
  "  {ul {li {em key list}: Lists known keys}",
  "      {li {em key access}: Shows the current access level}",
  "      {li {em key grant (none|read|execute|write|admin|almighty) <key-name>}: ",
  "          Grants access of given type to the current instance}",
  "      {li {em key set <key-name> <path>... = <value>}: Sets some metadata}",
  "      {li {em key meta <key-name> <path>...}: Shows a key's metadata}",
  "      {li {em key gen [client|server] <key-name>}: Generates a new private key}",
  "      {li {em key del [client|server] <key-name>}: Deletes a known key}",
  "      {li {em key export <key-name> [proof]}: Exports a claim for the given key, or a proof of it if specified}",
  "      {li {em key import <key-name> (<client-key-name>|#<export>)}: Imports an exported key under the given name}}",
  "}"
  ]
keyCmd = withDoc keyDoc $ False <$ do
  x <- expected "key command" (nbhspace >> dirArg)
  let setKey name v = do
        ks <- getKeyStore
        if name`isKeyIn`ks then serveStrLn (format "Error: the key '%s' already exists" name) >> zero
          else modifyKeyStore (at name %- Just v)
  case x of
    "access" -> serveStrLn $ format "You have %s access to this instance." (show ?access)
    "list" -> getKeyStore >>= \m -> do
      kl <- liftIO clientKeyList
      let inst = getConf confInstance
          showKey fp ids = format "%s%k: %s" 
                                 (case (openInstance,foldMap (by l'1) ids) of
                                    (True,_) -> ""
                                    (_,Deny) -> "[deny    ]"
                                    (_,Read) -> "[read    ]" ; (_,Run) -> "[execute ]" ; (_,Write) -> "[write   ]"
                                    (_,Admin) -> "[admin   ]" ; (_,Almighty) -> "[almighty]")
                                 fp
                                 (intercalate ", " [format "%s%s %s" tp (if isPriv then "proof" else "claim") name
                                                   | (_,name,tp,isPriv) <- ids]) 
          openInstance = Deny == foldMap (\(_,_,_,_,all) -> mlookup inst all) m
          fpAllowed = c'map $ fromMAList [(fp,mlookup inst all) | (_,(fp,_,_,_,all)) <- m^.ascList]
          decl (name,tp,fp,isPriv,isAll) = mat fp %~ ((isAll,name,tp,isPriv):)
      serveString $ unlines $ map (uncurry showKey) $ by ascList $ c'map $ foldr decl zero $ 
        [(name,"",f,has t'Just priv,mlookup inst all)
        | (name,(f,_,priv,_,all)) <- m^.ascList]
        +[(name,"client ",fp,priv,mlookup fp fpAllowed) | (name,fp,priv) <- kl]
    "gen" -> do
      isDistant <- option' True (nbhspace >> (False<$several "client" <+? True<$several "server"))
      name <- expected "key name" (nbhspace >> dirArg)
      if isDistant then
        if ?access>=Almighty then genPrivateKey >>= \k -> modifyKeyStore (insert name (let pub = publicKey k in (fingerprint pub,pub,Just k,zero,zero)))
        else serveStrLn "Error: you are not authorized to create server keys"
        else liftIOWarn $ clientKeyGen True name
    "del" -> do
      isDistant <- option' True (nbhspace >> (False<$several "client" <+? True<$several "server"))
      name <- expected "key name" (nbhspace >> dirArg)
      if isDistant then
        if ?access>=Almighty then modifyKeyStore (delete name)
        else serveStrLn "Error: you are not authorized to delete server keys"
        else liftIOWarn $ clientKeyGen False name
    "set" -> do
      name <- expected "key name" (nbhspace >> dirArg)
      ph:pt <- expected "metadata path" (many1' (nbhspace >> dirArg <*= \a -> guard (a/="=")))
      expected "keyword '='" (nbhspace >> single '=')
      value <- expected "value" (nbhspace >> many1' (noneOf "\n"))
      if ?access >= Almighty
        then modifyKeyStore $ at name.t'Just.l'4.mat ph %~ insert pt (Pure value)
        else serveStrLn "Error: you are not authorized to set key metadata"
    "meta" -> do
      name <- expected "key name" (nbhspace >> dirArg)
      path <- many' (nbhspace >> dirArg)
      mm <- getKeyStore <&> \ks -> ks^?at name.t'Just.l'4.getter (\(Metadata m) -> Join m).at path.t'Just
      maybe unit (serveStrLn . showMetaDir . mapF (\m -> ModDir (m^.ascList))) mm
    "grant" -> do
      tp <- expected "access type" (nbhspace >> (dirArg >*> readable))
      name <- expected "key name" (nbhspace >> dirArg)
      if ?access >= Admin && tp <= ?access then do
        modifyKeyStore $ at name %~ map (l'5.at (getConf confInstance).sat (\x -> fold x <= ?access)
                                         %- case tp of Deny -> Nothing ; _ -> Just tp)
        else serveStrLn "Error: you are not authorized to grant these permissions"
    "export" -> do
      name <- expected "key name" (nbhspace >> dirArg)
      proof <- option' False (nbhspace >> True<$several "proof")
      v <- lookup name <$> getKeyStore
      case v of
        Just (_,pub,priv,meta,_) -> serveStrLn (show (Zesty (pub,if proof && ?access >= Almighty then map (,meta) priv else Nothing)))
        Nothing -> serveStrLn ("Error: Unknown key '"+name+"'")
    "import" -> do
      name <- expected "key name" (nbhspace >> dirArg)
      try (serveStrLn "Error: Invalid key") $ expected "client key name or raw key export" $ do
        nbhspace
        Zesty (pub,priv) <- (single '#' >> dirArg) >*> readable
                            <+? Zesty . (,Nothing) <$> do
                              name' <- dirArg
                              logLine Debug $ format "Asking client for key '%s'" name'
                              (maybe zero return =<< liftIO (clientKey name'))
                                <+? (maybe zero (\(Zesty p) -> return p) =<< dns_lookup (DomainKey name'))
                                <+? (serveStrLn (format "Error: unknown client key '%s'" name') >> zero)
        let keyType = maybe "claim" (const "proof") priv
        serveStrLn (format "Importing %s '%s'" keyType name)
        setKey name (fingerprint pub,pub,map fst priv,maybe zero snd priv,zero)
    _ -> serveStrLn $ format "Error: unknown key command '%s'" x
