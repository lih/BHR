{-# LANGUAGE CPP #-}
module Graphics.Widget.Scrollable (
  -- * Adjustments
  Adjustment,lowerBound,upperBound,pageIncrement,stepIncrement,pageSize,
  -- * Scrollable widgets
  Scrollable,scrollable
  ) where

import Definitive
import Graphics.Widget.Internals

#include "definitive-graphics.h"

data GtkAdjustment
instance GObject GtkAdjustment

data Adjustment = Adjustment {
  _lowerBound,_upperBound,
  _stepIncrement,_pageIncrement,_pageSize 
                                :: Double
  }
                  deriving (Eq,Ord)
DEFFIELD_LENS(lowerBound) ; DEFFIELD_LENS(upperBound)
DEFFIELD_LENS(stepIncrement) ; DEFFIELD_LENS(pageIncrement)
DEFFIELD_LENS(pageSize)

data Scrollable a = Scrollable {
  _scrollableChild :: Widget a,
  _scrollableAdjustments :: (Adjustment,Adjustment),
  _scrollableCoords :: (Double,Double)
  }
HASPROP(ChildProp,Scrollable a,Widget a,FIELD_LENS(_scrollableChild))
HASPROP(AdjustmentsProp,Scrollable a,(Adjustment,Adjustment),FIELD_LENS(_scrollableAdjustments))
HASPROP(ViewportProp,Scrollable a,(Double,Double),FIELD_LENS(_scrollableCoords))

FOREIGN(gtk_adjustment_new,CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO (Ptr GtkAdjustment))
FOREIGN(gtk_adjustment_configure,Ptr GtkAdjustment -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO ())
FOREIGN(gtk_adjustment_get_value,Ptr GtkAdjustment -> IO CDouble)
FOREIGN(gtk_layout_new, Ptr GtkAdjustment -> Ptr GtkAdjustment -> IO (Ptr GtkWidget))
scrollable :: Widget a -> IO (Widget (Scrollable a))
scrollable w = mdo
  let defadj = Adjustment 0 100 5 25 25
      defcoord = 0
  hadjH <- newGObject (gtk_adjustment_new 0 0 100 5 25 25)
  vadjH <- newGObject (gtk_adjustment_new 0 0 100 5 25 25)
  h <- withForeignPtr hadjH $ \phadj -> withForeignPtr vadjH $ \pvadj -> newGObject (gtk_layout_new phadj pvadj)
  ret <- newWidget h (Scrollable w (defadj,defadj) (defcoord,defcoord)) <| do
    onChangeQual (Just "gtk") adjustments $ \_ (Adjustment lo up step pg pgSize,Adjustment lo' up' step' pg' pgSize') -> do
      (x,y) <- getDynamic ret viewport
      withForeignPtr hadjH $ \p -> gtk_adjustment_configure p (CDouble x) (CDouble lo) (CDouble up) (CDouble step) (CDouble pg) (CDouble pgSize)
      withForeignPtr vadjH $ \p -> gtk_adjustment_configure p (CDouble y) (CDouble lo') (CDouble up') (CDouble step') (CDouble pg') (CDouble pgSize')
  connectSignal hadjH "value-changed" =<< callback_pp_ $ \_ _ -> withForeignPtr hadjH $ \p -> do
    CDouble x <- gtk_adjustment_get_value p
    setDynamicQual (Just "gtk") ret (viewport.l'1) x
  connectSignal vadjH "value-changed" =<< callback_pp_ $ \_ _ -> withForeignPtr hadjH $ \p -> do
    CDouble y <- gtk_adjustment_get_value p
    setDynamicQual (Just "gtk") ret (viewport.l'2) y
  return ret
      
  
  
