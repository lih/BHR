module Curly.GUI2 where

import Definitive
import Graphics.Widget
import Graphics.Widget.GL.Scene
import Graphics.Widget.GL.Vertex

main = runApplication $ mdo
  b <- clickable (button (Just "Open")) <| do
    onHigh (lastClick 1.sat ((==Pressed) . by l'2)) $ \_ -> mdo
      fc <- fileChooser (const (pure True)) <| do
        onChange lastActivation $ \_ _ -> do
          setDynamic w visible False
          l <- getDynamic fc selected
          print l
      w <- dialog win "Open file..." [("Open",True),("Cancel",False)] fc
      setDynamic w visible True
  win <- window "Test window" b <| do visible =- True
  return ()
