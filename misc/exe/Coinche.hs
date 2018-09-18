{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module Main where

import Definitive
import Algebra.Monad.Concatenative
import System.Random.Shuffle

data Hauteur = H7 | H8 | H9 | HValet | HDame | HRoi | H10 | HAs
             deriving (Eq,Ord,Show,Enum)
data Couleur = Coeur | Carreau | Pique | Trefle
             deriving (Eq,Ord,Show,Enum)
data Carte = Carte Hauteur Couleur
instance Show Carte where
  show (Carte h c) = showH h +":"+ showC c
    where showH H7 = "7" ; showH H8 = "8" ; showH H9 = "9" ; showH H10 = "10"
          showH HValet = "J"; showH HDame = "Q"; showH HRoi = "K" ; showH HAs = "A"
          showC Coeur = "co"; showC Carreau = "ca" ; showC Pique = "pi";
          showC Trefle = "tr"

data CoincheState = CoincheState {
  _cards :: ([Carte],[Carte],[Carte],[Carte]),
  _halt :: Bool
  }

cards :: Lens' CoincheState ([Carte],[Carte],[Carte],[Carte])
cards = lens _cards (\x y -> x { _cards = y })
halt :: Lens' CoincheState Bool
halt = lens _halt (\x y -> x { _halt = y })

nomsHauteurs = c'map $ fromAList [(n,h)
                                 | (ns,h) <- [(["7","sept","seven"],H7),
                                              (["8"],H8),
                                              (["9"],H9),
                                              (["10"],H10),
                                              (["V","valet","J"],HValet),
                                              (["D","Q","dame"],HDame),
                                              (["R","roi","K"],HRoi),
                                              (["A","as"],HAs)]
                                 , n <- ns]
nomsCouleurs = c'map $ fromAList [(n,h)
                                 | (ns,h) <- [(["carreau"],Carreau),
                                              (["coeur"],Coeur),
                                              (["trefle"],Trefle),
                                              (["pique"],Pique)]
                                 , n <- ns]


data Builtin = Quit | Hands | Deal | Show | MkCarte
             deriving Show
runBuiltin Quit = runExtraState (halt =- True)
runBuiltin Hands = do
  (p1,p2,p3,p4) <- runExtraState (getl cards)
  for_ (zip [1..4] [p1,p2,p3,p4]) $ \(i,pi') -> lift $ do
    putStrLn $ "Player "+show i+": "+intercalate " " (map show pi')
runBuiltin Deal = do
  cs <- lift $ shuffleM deck
  let (h1,cs1) = splitAt 8 cs
      (h2,cs2) = splitAt 8 cs1
      (h3,h4)  = splitAt 8 cs2
  runExtraState $ do cards =- (h1,h2,h3,h4)
runBuiltin Show = do
  st <- runStackState get
  case st of
    StackList l:_ -> do
      lift $ putStrLn $ intercalate " " (map show l)
      runStackState (modify tail)
    StackExtra (Opaque c):_ -> do
      lift $ putStrLn $ show c
      runStackState (modify tail)
    _ -> return ()
runBuiltin MkCarte = runStackState $ modify $ \case
  StackSymbol c:StackSymbol h:t | Just h' <- lookup h nomsHauteurs
                                , Just c' <- lookup c nomsCouleurs -> StackExtra (Opaque (Carte h' c')):t
  st -> st

deck = [Carte h c | h <- [H7 .. HAs], c <- [Coeur .. Trefle]]

main = do
  str <- words <$> getContents
  execS (foldr (\sym mr -> do
                   execSymbol runBuiltin sym
                   hasQuit <- runExtraState (getl halt)
                   unless hasQuit mr
               ) unit str^..concatT) (defaultState (fromAList [(x,StackBuiltin b)
                                                              | (x,b) <- [("quit",Builtin_Extra Quit),("show",Builtin_Extra Show),("hands",Builtin_Extra Hands),("deal",Builtin_Extra Deal),("def",Builtin_Def),("pop",Builtin_Pop),("$",Builtin_DeRef),("exec",Builtin_Exec)]])
                                       (CoincheState ([],[],[],[]) False))
