{-# LANGUAGE CPP #-}
module Graphics.Widget.Switch (
  switch2,switch3,switch4,switch5,switch6,switch7,switch8,switch9
  ) where

import Definitive
import Graphics.Widget.Internals
import Graphics.Widget.Containers.Internal

#include "definitive-graphics.h"

setW :: ForeignPtr GtkWidget -> Widget a -> IO ()
setW h w = boxAddWidget h 0 =<< boxChild (pure w)

type SwitchConstructor t = t -> IO (Widget t)

mkSwitch :: Eq t => (forall d. (forall x. Widget x -> d) -> t -> d) -> SwitchConstructor t
mkSwitch unUnion x = do
  h <- newGObject (newGtkBox Horizontal)
  unUnion (setW h) x
  newWidget h x <| do onChange traitValue $ \c c' -> withForeignPtr h $ \p -> do
                        unUnion withGtkWidget c (gtk_container_remove p)
                        unUnion (setW h) c'
                        gtk_widget_show_all p

-- | A widget which can dynamically swap between two alternatives
switch2 :: (Widget a:+:Widget b) -> IO (Widget (Widget a:+:Widget b))
switch2 = mkSwitch $ \f -> f<|>f
switch3 :: SwitchConstructor (Union3 (Widget a) (Widget b) (Widget c))
switch3 = mkSwitch $ \f o -> case o of U3_1 x -> f x; U3_2 x -> f x; U3_3 x -> f x
switch4 :: SwitchConstructor (Union4 (Widget a) (Widget b) (Widget c) (Widget d))
switch4 = mkSwitch $ \f o -> case o of U4_1 x -> f x; U4_2 x -> f x; U4_3 x -> f x; U4_4 x -> f x
switch5 :: SwitchConstructor (Union5 (Widget a) (Widget b) (Widget c) (Widget d) (Widget e))
switch5 = mkSwitch $ \f o -> case o of U5_1 x -> f x; U5_2 x -> f x; U5_3 x -> f x; U5_4 x -> f x; U5_5 x -> f x
switch6 :: SwitchConstructor (Union6 (Widget a) (Widget b) (Widget c) (Widget d) (Widget e) (Widget f))
switch6 = mkSwitch $ \f o -> case o of U6_1 x -> f x; U6_2 x -> f x; U6_3 x -> f x; U6_4 x -> f x; U6_5 x -> f x; U6_6 x -> f x
switch7 :: SwitchConstructor (Union7 (Widget a) (Widget b) (Widget c) (Widget d) (Widget e) (Widget f) (Widget g))
switch7 = mkSwitch $ \f o -> case o of U7_1 x -> f x; U7_2 x -> f x; U7_3 x -> f x; U7_4 x -> f x; U7_5 x -> f x; U7_6 x -> f x; U7_7 x -> f x
switch8 :: SwitchConstructor (Union8 (Widget a) (Widget b) (Widget c) (Widget d) (Widget e) (Widget f) (Widget g) (Widget h))
switch8 = mkSwitch $ \f o -> case o of U8_1 x -> f x; U8_2 x -> f x; U8_3 x -> f x; U8_4 x -> f x; U8_5 x -> f x; U8_6 x -> f x; U8_7 x -> f x; U8_8 x -> f x
switch9 :: SwitchConstructor (Union9 (Widget a) (Widget b) (Widget c) (Widget d) (Widget e) (Widget f) (Widget g) (Widget h) (Widget i))
switch9 = mkSwitch $ \f o -> case o of U9_1 x -> f x; U9_2 x -> f x; U9_3 x -> f x; U9_4 x -> f x; U9_5 x -> f x; U9_6 x -> f x; U9_7 x -> f x; U9_8 x -> f x; U9_9 x -> f x
