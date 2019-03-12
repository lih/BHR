{-# LANGUAGE CPP, UndecidableInstances #-}
module Graphics.Widget.Grid(
  Grid,gridN
  ) where

import Definitive
import Graphics.Widget.Internals
import Graphics.Widget.Containers.Internal

#include "definitive-graphics.h"

data Grid t = Grid {
  _gridTuple :: t
  }

gridTuple :: Lens t t' (Grid t) (Grid t')
gridTuple = FIELD_LENS(_gridTuple)
instance Lens1 a a t t => DynamicProperty (Grid t) a Child0Prop where
  property Child0Prop = gridTuple.l'1
instance Lens2 a a t t => DynamicProperty (Grid t) a Child1Prop where
  property Child1Prop = gridTuple.l'2
instance Lens3 a a t t => DynamicProperty (Grid t) a Child2Prop where
  property Child2Prop = gridTuple.l'3
instance Lens4 a a t t => DynamicProperty (Grid t) a Child3Prop where
  property Child3Prop = gridTuple.l'4
instance DynamicProperty (Grid t) t ChildrenProp where
  property ChildrenProp = gridTuple

type GridWidget a = Widget (GridChild a)

FOREIGN(gtk_grid_new, IO (Ptr GtkWidget))
FOREIGN(gtk_grid_attach, Ptr GtkWidget -> Ptr GtkWidget -> CInt -> CInt -> CInt -> CInt -> IO ())
gridN :: [GridWidget w] -> IO (Widget (Grid [GridWidget w]))
gridN cs = do
  wid <- newGObject gtk_grid_new
  let attach cs = for_ cs $ \c -> do
        (l,t,w,h) <- getDynamic c packing
        withGtkWidget c $ \cp -> withForeignPtr wid $ \wp -> do
          gtk_grid_attach wp cp (fromIntegral l) (fromIntegral t) (fromIntegral w) (fromIntegral h)
  attach cs
  newWidget wid (Grid cs) <| do
    onChange children $ \cs cs' -> withForeignPtr wid $ \wp -> do
      for_ cs $ \c -> withGtkWidget c (gtk_container_remove wp)
      attach cs'

                                                          
