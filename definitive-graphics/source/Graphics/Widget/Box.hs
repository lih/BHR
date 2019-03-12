{-# LANGUAGE CPP, UndecidableInstances #-}
module Graphics.Widget.Box(
  HList,Box,Orientation(..),boxN,box2,box3,box4,box5,box6,box7,box8,box9
  ) where

import Definitive
import Graphics.Widget.Internals
import Graphics.Widget.Containers.Internal

#include "definitive-graphics.h"

data HList w = HList {
  _children :: [SubWidget w]
  }
HASPROP(ChildrenProp,HList w,[SubWidget w],FIELD_LENS(_children))
data Box t = Box {
  _tupleOrientation :: Orientation,
  _tupleValue :: t
  }

instance DynamicProperty (Box t) t ChildrenProp where
  property ChildrenProp = FIELD_LENS(_tupleValue)
instance Lens1 a a t t => DynamicProperty (Box t) a Child0Prop where
  property Child0Prop = children.l'1
instance Lens2 a a t t => DynamicProperty (Box t) a Child1Prop where
  property Child1Prop = children.l'2
instance Lens3 a a t t => DynamicProperty (Box t) a Child2Prop where
  property Child2Prop = children.l'3
instance Lens4 a a t t => DynamicProperty (Box t) a Child3Prop where
  property Child3Prop = children.l'4

boxN :: Orientation -> [SubWidget w] -> IO (Widget (HList w))
boxN o ws = do
  h <- newGObject (newGtkBox o)
  sequence_ [boxAddWidget h n w | (n,w) <- zip [0..] ws]
  newWidget h (HList ws) <| do
    onChange children $ \cs cs' -> withForeignPtr h $ \p -> do
      sequence_ [withGtkWidget w (gtk_container_remove p) | w <- cs]
      sequence_ [boxAddWidget h n w | (n,w) <- zip [0..] cs']
      gtk_widget_show_all p

data BoxField t = forall a. BoxField (Lens' t (SubWidget a))

mkBox :: [BoxField t] -> Orientation -> t -> IO (Widget (Box t))
mkBox attrs o x = do
  h <- newGObject (newGtkBox o)
  sequence_ [boxAddWidget h i (x^.l) | (i,BoxField l) <- zip [0..] attrs]
  newWidget h (Box o x) <| do
    sequence_ [onChange (children.l) (swapChild h i) | (i,BoxField l) <- zip [0..] attrs]

box2 o a b = mkBox [BoxField l'1,BoxField l'2] o (a,b)
box3 o a b c = mkBox [BoxField l'1,BoxField l'2,BoxField l'3] o (a,b,c)
box4 o a b c d = mkBox [BoxField l'1,BoxField l'2,BoxField l'3,BoxField l'4] o (a,b,c,d)
box5 o a b c d e = mkBox [BoxField l'1,BoxField l'2,BoxField l'3,BoxField l'4,BoxField l'5] o (a,b,c,d,e)
box6 o a b c d e f = mkBox [BoxField l'1,BoxField l'2,BoxField l'3,BoxField l'4,BoxField l'5,BoxField l'6] o (a,b,c,d,e,f)
box7 o a b c d e f g = mkBox [BoxField l'1,BoxField l'2,BoxField l'3,BoxField l'4,BoxField l'5,BoxField l'6,BoxField l'7] o (a,b,c,d,e,f,g)
box8 o a b c d e f g h = mkBox [BoxField l'1,BoxField l'2,BoxField l'3,BoxField l'4,BoxField l'5,BoxField l'6,BoxField l'7,BoxField l'8] o (a,b,c,d,e,f,g,h)
box9 o a b c d e f g h i = mkBox [BoxField l'1,BoxField l'2,BoxField l'3,BoxField l'4,BoxField l'5,BoxField l'6,BoxField l'7,BoxField l'8,BoxField l'9] o (a,b,c,d,e,f,g,h,i)
