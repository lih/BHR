{-# LANGUAGE TypeFamilies, PatternSynonyms #-}
module Console.Readline (readline,addHistory,setCompletionEntryFunction) where

import Definitive
import Language.Parser
import System.IO (hSetEcho,hSetBuffering,BufferMode(..),openFile,IOMode(..))
import Data.IORef
import qualified System.Console.Terminal.Size as TSize
import Control.DeepSeq (($!!))
import Control.Exception (bracket_)

tty = (openFile "/dev/tty" ReadWriteMode <*= \h -> hSetBuffering h NoBuffering)^.thunk

data ControlSequence = CSI [Int] Char
                     | Esc Char
                     | RawChar Char

class TTYCommand t where
  _putTTY :: t -> IO ()
instance TTYCommand Char where
  _putTTY c = writeHString tty [c]
instance TTYCommand ControlSequence where
  _putTTY = writeHString tty . showCS
    where showCS (RawChar c) = [c]
          showCS (CSI args c) = "\ESC["+intercalate ";" (map show args)+[c]
          showCS (Esc c) = ['\ESC',c]
instance TTYCommand c => TTYCommand [c] where
  _putTTY l = traverse_ putTTY l

putTTY :: (TTYCommand t,MonadIO m) => t -> m ()
putTTY = liftIO . _putTTY

pattern CUU n = CSI n 'A'
pattern CUD n = CSI n 'B'
pattern CUF n = CSI n 'C'
pattern CUB n = CSI n 'D'

cseq :: (ParseStream s, MonadParser s m p, StreamChar s ~ Char) => p ControlSequence
cseq = esc <+? map RawChar token
  where esc = do
          single '\ESC'
          (single '[' >> csi) <+? (token >>= \case
                                      'O' -> CSI [] <$> token
                                      'P' -> CSI [] <$> token
                                      x -> return (Esc x))
        csi = liftA2 CSI (sepBy' number (single ';')) token

data RLState = RLState {
  _rlPrefix, _rlSuffix :: String,
  _rlHistory, _rlFuture :: [String]
  }

rlPrefix :: Lens' RLState String
rlPrefix = lens _rlPrefix (\x y -> x { _rlPrefix = y })
rlSuffix :: Lens' RLState String
rlSuffix = lens _rlSuffix (\x y -> x { _rlSuffix = y })
rlHistory :: Lens' RLState [String]
rlHistory = lens _rlHistory (\x y -> x { _rlHistory = y })
rlFuture :: Lens' RLState [String]
rlFuture = lens _rlFuture (\x y -> x { _rlFuture = y })

rl_stateref = (readHString tty >>= \s -> newIORef (s,[],\_ -> return []))^.thunk

readline :: String -> IO (Maybe String)
readline prompt = bracket_ (hSetEcho tty False) (hSetEcho tty True) $ do
  writeHString tty prompt
  (inp,hist,complete) <- readIORef rl_stateref
  (st',l) <- ((axiom complete^..parserT) inp^..stateT) (RLState zero zero hist zero)
  case l of
    [(rem,s)] -> do
      writeIORef rl_stateref (rem,hist,complete)
      return (Just s)
    _ -> return Nothing
  where axiom complete = axiom
          where
            forwardChar n = do
              doMove <- lift $ do (p,s) <- liftA2 (,) (getl rlPrefix) (getl rlSuffix)
                                  let (h,s') = splitAt n s
                                  rlPrefix =- reverse h+p
                                  rlSuffix =- s'
                                  return (length h)
              when (doMove > 0) $ putTTY (CUF [doMove])
            backwardChar n = do
              doMove <- lift $ do (p,s) <- liftA2 (,) (getl rlPrefix) (getl rlSuffix)
                                  let (h,p') = splitAt n p
                                  rlPrefix =- p'
                                  rlSuffix =- reverse h+s
                                  return (length h)
              when (doMove > 0) $ putTTY (CUB [doMove])
            deleteBackward n = do
              (pref,suf) <- lift $ liftA2 (,) (getl rlPrefix) $ do
                rlPrefix =~ drop n; getl rlSuffix
              let prefl = length (take n pref)
              unless (prefl == 0) $ do putTTY (CUB [prefl])
                                       hidingCursor $ do
                                         putTTY (suf+take prefl (repeat ' ')) ; putTTY (CUB [prefl+length suf])
            deleteForward n = do -- Reverse delete
              suf <- lift $ getl rlSuffix <* do rlSuffix =~ drop n
              let sufl = length (take n suf)
              unless (sufl == 0) $ hidingCursor $ do putTTY (drop sufl suf+take sufl (repeat ' '))
                                                     putTTY (CUB [length suf])
            
            insertText txt = do
              suf <- lift $ do rlPrefix =~ (txt+); getl rlSuffix
              putTTY txt
              hidingCursor (putTTY suf >> unless (empty suf) (putTTY (CUB [length suf])))

            hidingCursor m = putTTY [Esc '['] >> putTTY "?25l" >> m <* putTTY [Esc '['] <* putTTY "?25h" 

            histPrev = do
              doMove <- lift $ do RLState p s h f <- get
                                  case h of
                                    [] -> return Nothing
                                    prev:h' -> do
                                      put (RLState (reverse prev) "" h' ((reverse p+s):f))
                                      return (Just (length p+length s,prev))
              case doMove of
                Just (wh,prev) -> do putTTY [RawChar '\r',CUF [length prompt]] >> putTTY (take wh (repeat ' '))
                                     putTTY [RawChar '\r',CUF [length prompt]] >> putTTY prev
                Nothing -> return ()
            histNext = do
              doMove <- lift $ do RLState p s h f <- get
                                  case f of
                                    [] -> return Nothing
                                    next:f' -> do
                                      put (RLState (reverse next) "" ((reverse p+s):h) f')
                                      return (Just (length p+length s,next))
              case doMove of
                Just (wh,prev) -> do putTTY [RawChar '\r',CUF [length prompt]] >> putTTY (take wh (repeat ' '))
                                     putTTY [RawChar '\r',CUF [length prompt]] >> putTTY prev
                Nothing -> return ()

            axiom = do
              c <- cseq
              case c of
                RawChar '\EOT' -> do
                  emptyline <- lift $ liftA2 (\x y -> empty x && empty y) (getl rlPrefix) (getl rlSuffix)
                  if emptyline then zero
                    else deleteForward 1 >> axiom
              
                RawChar '\n' -> do
                  putTTY c
                  lift $ liftA2 (\x y -> reverse x+y) (getl rlPrefix) (getl rlSuffix)

                RawChar '\t' -> do
                  pref <- lift (getl rlPrefix)
                  comps <- liftIO $ try (return []) $ (return$!!) =<< complete (reverse pref)
                  let commonPrefix (x:xs) (y:ys) | x==y = x:commonPrefix xs ys
                      commonPrefix _ _ = []
                      withoutSuffix l@(x:t) s | length (commonPrefix l s) /= length l = x:withoutSuffix t s
                      withoutSuffix _ _ = []
                  additional <- case comps of
                    [] -> putTTY '\BEL' >> return Nothing
                    [w] -> do
                      h <- lift $ do p <- getl rlPrefix
                                     let h = withoutSuffix (reverse w) p
                                     rlPrefix =- ' ':h+p
                                     (h,) <$> getl rlSuffix
                      return (Just (first (' ':) h))
                    ws -> do
                      tw <- liftIO (maybe 80 TSize.width <$> TSize.size)
                      let w = foldr1 commonPrefix ws
                          colSize = 1+maximum (map length ws)
                          ncols = tw `div` colSize
                          groupsOf n [] = []
                          groupsOf n l = let (h,t) = splitAt n l in h:groupsOf n t
                          padTo n s = take n (s + repeat ' ')
                      (h,p,s) <- lift $ do p <- getl rlPrefix
                                           let h = withoutSuffix (reverse w) p
                                           rlPrefix =- h+p
                                           (h,p,)<$>getl rlSuffix
                      putTTY ("\n"+intercalate "\n" (map (foldMap (padTo colSize)) (groupsOf ncols ws))+
                              "\n"+prompt+reverse p+s)
                      unless (empty s) (putTTY (CUB [length s]))
                      return (Just (h,s))
                  case additional of
                    Just (word,suf) -> putTTY (reverse word + suf) >> unless (empty suf) (putTTY (CUB [length suf]))
                    Nothing -> unit
                  axiom

                RawChar '\DEL' -> deleteBackward 1 >> axiom
                CSI [3] '~' -> deleteForward 1 >> axiom
                
                RawChar '\SOH' -> do -- Ctrl-a to beginning of line
                  lift $ modify (\(RLState p s h f) -> RLState "" (reverse p+s) h f)
                  putTTY '\r' >> putTTY (CUF [length prompt])
                  axiom
                RawChar '\ENQ' -> do -- Ctrl-e to end of line
                  sz <- lift $ (\(RLState p s h f) -> return (RLState (reverse s+p) "" h f,length s+length p))^.stateT
                  putTTY '\r' >> putTTY (CUF [length prompt+sz])
                  axiom
              
                RawChar '\v' -> do -- Ctrl-k kills the end-of-line
                  suf <- lift $ (\(RLState p s h f) -> return (RLState p "" h f,s))^.stateT
                  putTTY (take (length suf) (repeat ' ')) >> unless (empty suf) (putTTY (CUB [length suf]))
                  axiom
                RawChar '\f' -> -- ignore Ctrl-l
                  axiom
              
                CUB [] -> backwardChar 1 >> axiom
                RawChar '\STX' -> backwardChar 1 >> axiom
                Esc 'b' -> do
                  pre <- lift (getl rlPrefix)
                  backwardChar (length (takeWhile (\x -> not (x`elem`" \t")) pre))
                  axiom
                
                RawChar '\ACK' -> forwardChar 1 >> axiom
                CUF [] -> forwardChar 1 >> axiom
                Esc 'f' -> do
                  suf <- lift (getl rlSuffix)
                  forwardChar (length (takeWhile (\x -> not (x`elem`" \t")) suf))
                  axiom
                
                CUU [] -> histPrev >> axiom
                CUU [0] -> histPrev >> axiom
                CUU [1] -> histPrev >> axiom
                CUD [] -> histNext >> axiom
                CUD [0] -> histNext >> axiom
                CUD [1] -> histNext >> axiom
              
                RawChar c -> insertText [c] >> axiom
                
                CSI args c -> insertText ("^[["+intercalate ";" (map show args)+[c]) >> axiom
                Esc c -> insertText ("^["+[c]) >> axiom
              
addHistory :: String -> IO ()
addHistory cmd = modifyIORef rl_stateref (warp l'2 (cmd:))

setCompletionEntryFunction :: Maybe (String -> IO [String]) -> IO ()
setCompletionEntryFunction f = modifyIORef rl_stateref (set l'3 (fromMaybe (\_ -> return []) f))
