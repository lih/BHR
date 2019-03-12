module Main where

import Definitive
import Graphics.Widget
import System.Environment
import Data.IORef
import Curly.Core
import Curly.Core.Documentation
import Curly.Core.Peers
import Curly.Core.Library
import Curly.Core.VCS
import Curly.Core.Security
import Curly.UI
import Curly.Style
import IO.Network.Socket
import Language.Format
import Paths_curly_gui

showLeafDoc x = do
  term <- setupTermFromEnv
  putStrLn $ docString term ?style (x^.leafDoc)
  
main = let ?style = defaultStyle in void $ runApplication $ mdo
  initCurly getDataFileName
  let nextTab delta = runDynamicState display $ do
        l <- getl children
        when (nonempty l) $ do selected =~ \n -> (n+delta)`mod`length l
      notebookLabel deleteTab txt = do
        img <- (boxChild.clickable) (image (Icon IS_SmallToolbar "window-close")) <| do
          packing =~ set expands Fitting . set padding 4
          onChange (lastClick 1) $ \_ _ -> deleteTab
        lbl <- (boxChild.clickable) (text txt) <| do
          onChange (lastClick 3) $ \_ _ -> do
            mi <- accelMenuItem "Close tab" <| do onChange lastActivation $ \_ _ -> deleteTab
            m <- menu [mi]
            setDynamic m visible True
        hideable (box2 Horizontal lbl img) <| do visible =- True

      addTab x = runDynamicState display (children =~ (+[x]))
      delTab x = runDynamicState display (children =~ refuse (==x))
  
  runDynamicState ?application $ do
    shortcuts =~ insert "<Ctrl>q" (Assoc "Quit" quitApplication)
    shortcuts =~ insert "<Ctrl>l" (Assoc "Go to address" $ do
                                      setDynamic addressBar hasFocus True)
    shortcuts =~ insert "<Ctrl>w" (Assoc "Delete current tab" $ do
                                      l <- runDynamicState display $ do
                                        i <- getl selected
                                        children `swapWith` \l -> let (h,t) = splitAt i l
                                                                  in h+drop 1 t
                                      when (empty l) quitApplication)
    shortcuts =~ insert "<Ctrl>o" (Assoc "Open file" $ do
                                      setDynamic fcDialog visible True)
    shortcuts =~ insert "<Ctrl>Page_Up" (Assoc "Next tab" (nextTab 1))
    shortcuts =~ insert "<Ctrl>Page_Down" (Assoc "Previous tab" (nextTab (-1)))
    shortcuts =~ insert "<Ctrl>r" (Assoc "Manage repositories" $ do
                                      setDynamic repoDialog visible True)
    shortcuts =~ insert "<Ctrl>t" (Assoc "Switch to next tab" $ do
                                      runDynamicState display $ do
                                        cs <- getl children
                                        selected =~ \i -> (i+1) `mod` length cs)
    
  addressBar <- (boxChild.focusable.keyboardEnabled) textInput <| do
    hasFocus =- True
    let updateCombo = do
          h <- getDynamic addressBar label
          insts <- try (return []) $ do
            h <- connectTo h curlyPort
            fold <$> runConnection Just True h (exchange AskInstances)
          runDynamicState instanceChoice $ do alternatives =- insts
                                              selected =- 0
    onHigh (lastKey GDK_KEY_Return . sat (\(_,x) -> x==Pressed)) $ \_ -> updateCombo
    onHigh (hasFocus.sat not) $ \_ -> updateCombo
  instanceChoice <- boxChild (comboBox []) 

  launchButton <- (boxChild.clickable.keyboardEnabled) (button (Just "Launch")) <| do
    packing.expands =- Fitting
    let spawnChild = mdo
          h <- getDynamic addressBar label
          is <- getDynamic instanceChoice alternatives
          s' <- getDynamic instanceChoice selected
          let i = fromMaybe "" (zip [0..] is^.at s')
              tc@(prog,args) = ("/usr/bin/curly",["--at",h+"/"+i,"--interactive"])
              deleteTab = delTab (hd,t)
          t <- switch3 . U3_1 =<< terminal <| do terminalStatus =- CommandTerminal tc
                                                 onHigh (terminalStatus.sat (==IdleTerminal)) $ \_ -> deleteTab
          hd <- notebookLabel deleteTab ("Session:"+h+"/"+i)
          runDynamicState display $ do children =~ (+[(hd,t)])
    onHigh clicked $ \_ -> spawnChild
  libsButton <- (boxChild.clickable.keyboardEnabled) (button (Just "Libraries")) <| do
    packing.expands =- Fitting
    let spawnChild = mdo
          let deleteTab = delTab (hd,c)
          libs <- availableLibs
          labels <- for libs $ \(lid,meta) -> do
            let s = fromMaybe "" (meta^?at "synopsis".t'Just.t'Pure)
            def <- switch2 . Left =<< text "Loading..."
            boxChild (expander (show lid+": "+s) def) <*= \e -> runDynamicState e $ do
              packing.expands =- Fitting
              onHigh (expanded.sat id) $ \_ -> do
                case findLib lid of
                  Just l -> setDynamic def traitValue . Right =<< contextW (l^.flLibrary.exports) showLeafDoc
                  Nothing -> putStrLn $ "Couldn't find library "+show lid
          c <- switch3 . U3_2 =<< textFrame "Libraries in repositories" =<< boxN Vertical labels
          hd <- notebookLabel deleteTab "All Libraries"
          runDynamicState display $ do children =~ (+[(hd,c)])
    onHigh clicked $ \_ -> spawnChild
  sep <- boxChild (separator Vertical) <| do packing.expands =- Fitting
  display <- boxChild notebook

  bar <- (boxChild.mkMenuBar) [subMenu "File" [accelMenuItem "Close tab" <| do shortcuts =- Just "Delete current tab"
                                              ,accelMenuItem "Open file..." <| do shortcuts =- Just "Open file"
                                              ,accelMenuItem "Manage repositories..." <| do shortcuts =- Just "Manage repositories"
                                              ,accelMenuItem "Quit"  <| do shortcuts =- Just "Quit"]]
  runDynamicState bar (packing.expands =- Fitting)
  
  actionFrame <- boxChild (textFrame "Actions" =<< box2 Horizontal launchButton libsButton) <| do packing.expands =- Fitting
                                                                                                  packing.padding =- 4
  addressFrame <- boxChild (textFrame "Address" =<< box2 Horizontal addressBar instanceChoice)
    
  top <- boxChild (box2 Horizontal addressFrame actionFrame) <| do packing.expands =- Fitting
  contents <- box4 Vertical bar top sep display
  win <- window "The Curly GUI" contents

  fchoose <- fileChooser (const (const True))
  fcDialog <- mfix $ \fcDialog -> do
    let openSelected = do
          sel <- getDynamic fchoose selected
          withCurlyConfig (map Left sel) $ withCurlyPlex ?curlyConfig $ withMountain $ mdo
            w <- switch3 . U3_3 =<< contextW localContext showLeafDoc
            hd <- notebookLabel (delTab (hd,w)) ("Context:"+head sel)
            addTab (hd,w)
        hideDialog = setDynamic fcDialog visible False
    runDynamicState fchoose $ do
      onChange lastActivation $ \_ _ -> openSelected >> hideDialog
    dialog win "Open file" [("Open",True),("Cancel",False)] fchoose <| do
      onChange lastActivation $ \_ (_,res) -> when res openSelected >> hideDialog

  repoDialog <- mdo
    ks <- getKeyStore
    keyId <- (boxChild.clickable.keyboardEnabled) (comboBox (keys ks)) <| do
      packing.expands =- Floating
      onChange selected $ \_ sel -> do
        key <- (!!sel) <$> getDynamic keyId alternatives
        StampedBranches _ bs <- getVCSBranches key
        setDynamic branchName alternatives (keys bs)
    branchName <- (boxChild.clickable.keyboardEnabled) (comboBox []) <| do packing.expands =- Fitting
    ret <- window "Manage repositories" =<< textFrame "VC repository" =<< box2 Horizontal keyId branchName
    return ret <| do visible =- False
    
  return ()

i'Free :: (Widget (f (FreeW f a)):+:Widget a) :<->: FreeW f a
i'Free = iso (Join . Compose . Compose <|> Pure)
         (\x -> case x of
            Join (Compose (Compose x)) -> Left x
            Pure x -> Right x)

type FreeW f a = Free (Dynamic:.:WProps:.:f) (Widget a)
freeW :: FreeW f a -> IO (Widget (FreeW f a))
freeW f = isoTrait (from (mapping i'Free)) (switch2 (f^..i'Free))

newtype CWNode a = CWNode (Box (SubWidget Separator,SubWidget (HList (Expander a))))
 
contextW :: Context -> (LeafExpr GlobalID -> IO ()) -> IO (Widget (FreeW CWNode (Box (SubWidget Separator,SubWidget Button))))
contextW (Pure (s,e)) expr = freeW . Pure =<< do
  t <- boxChild (button (Just (pretty s))) <| do
    packing.expands =- Fitting
    onChange lastActivation $ \_ _ -> print "Clicked" >> expr e
  sep <- boxChild (separator Vertical) <| do packing =~ set expands Fitting . set padding 15
  box2 Horizontal sep t
contextW (Join (ModDir l)) k = do
  subs <- for l $ \(s,a) -> boxChild (expander s =<< contextW a k) <| do packing.expands =- Fitting
  sep <- boxChild (separator Vertical) <| do packing.expands =- Fitting; packing.padding =- 15
  c <- boxChild (boxN Vertical subs)
  e <- isoTrait (mapping (iso (\(CWNode x) -> x) CWNode)) (box2 Horizontal sep c)
  freeW (Join (Compose $ Compose e))

lastClickOrKey :: (DynamicProperty w ClickMap LastClicksProp
                   ,DynamicProperty w KeyMap LastKeysProp) => [KeyCode] -> FixFold' w (Seconds,ClickType)
lastClickOrKey ks cc w = w <$ cc (ktm,kt)
  where (ktm,kt) = foldl' max (zero,Released) $
                   [w^.lastKeys.at k.l'Just (zero :: Seconds,Released) | k <- ks]
                   + [(tm,t) | (_,(tm,t,_,_)) <- w^.lastClicks.ascList]

clicked :: (DynamicProperty w ClickMap LastClicksProp
           ,DynamicProperty w KeyMap LastKeysProp) => FixFold' w Seconds
clicked = lastClickOrKey [GDK_KEY_Return,GDK_KEY_space].sat (\(_,x) -> x==Released).l'1

subMenu n l = do
  c <- menu =<< sequence l
  accelMenuItem n <| (children =- Just c)
mkMenuBar lm = menuBar =<< sequence lm
