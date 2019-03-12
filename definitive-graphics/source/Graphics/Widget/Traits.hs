{-# LANGUAGE CPP, UndecidableInstances #-}
module Graphics.Widget.Traits(
  -- * Types
  module Graphics.GDK.KeyCodes,
  GtkWidget,Widget,Seconds,
  WProps(..),
  ClickType(..),ClickMap,Clickable(..),
  Focusable(..),
  KeyMap,KeyboardEnabled(..),
  Expands(..),BoxPacking,BoxChild(..),
  GridPacking,GridChild(..),
  Hideable(..),
  SubWidget,
  -- * Properties
  LastClicksProp(..),lastClicks,lastClick,
  VisibleProp(..),visible,
  PackingProp(..),packing,expands,padding,packAtEnd,
  LastKeysProp(..),lastKeys,lastKey,
  HasFocusProp(..),hasFocus,
  GTKHandleProp(..),gtkHandle
  ) where

import Definitive
import Foreign.Ptr
import Foreign.ForeignPtr
import IO.Time (Seconds)
import Graphics.GDK.KeyCodes
import IO.Dynamic

#include "definitive-graphics.h"

data GtkWidget

DEFCALLBACK(p_,Ptr a -> IO ())

data WProps a = WProps {
  _wHandle :: ForeignPtr GtkWidget,
  _wPropsValue :: a
  }
nullWidgetPtr = thunk $^ do
  cb <- callback_p_ (const unit)
  newForeignPtr cb nullPtr
instance Functor WProps where map f (WProps h a) = WProps h (f a)
instance Unit WProps where pure x = WProps nullWidgetPtr x
instance SemiApplicative WProps where WProps h f <*> WProps h' x = WProps (max h h') (f x)
instance Applicative WProps
type Widget a = Dynamic (WProps a)
instance Eq (WProps a) where p == p' = _wHandle p == _wHandle p'
instance Ord (WProps a) where compare = comparing _wHandle
instance Show (WProps a) where show (WProps h _) = show h

data ClickType = Pressed | Released
               deriving (Enum,Eq,Ord,Show)
type ClickMap = Map Int (Seconds,ClickType,Int,Int)
data Clickable a = Clickable {
  _lastClicks :: ClickMap,
  _clickableValue :: a
  }
data Focusable a = Focusable {
  _hasFocus :: Bool,
  _focusableValue :: a
  }
type KeyMap = Map KeyCode (Seconds,ClickType)
data KeyboardEnabled a = KeyboardEnabled {
  _lastKeys :: KeyMap,
  _keyboardEnabledValue :: a
  }
data Hideable a = Hideable {
  _hideableVisible :: Bool,
  _hideableValue :: a
  }
data Expands = Fitting | Floating | Expanding
             deriving Eq
type BoxPacking = (Expands,Bool,Int)
data BoxChild a = BoxChild {
  _boxChildPacking :: BoxPacking,
  _boxChildValue :: a
  }
type SubWidget a = Widget (BoxChild a)

type GridPacking = (Int,Int,Int,Int)
data GridChild a = GridChild {
  _gridChildPacking :: GridPacking,
  _gridChildValue :: a
  }

instance Trait Hideable        where traitValue = FIELD_LENS(_hideableValue)
instance Trait Clickable       where traitValue = FIELD_LENS(_clickableValue)
instance Trait BoxChild        where traitValue = FIELD_LENS(_boxChildValue)
instance Trait Focusable       where traitValue = FIELD_LENS(_focusableValue)
instance Trait KeyboardEnabled where traitValue = FIELD_LENS(_keyboardEnabledValue)
instance Trait WProps          where traitValue = FIELD_LENS(_wPropsValue)
instance Trait GridChild       where traitValue = FIELD_LENS(_gridChildValue)

#undef IFN_Clickable
#define IFN_Clickable(x...)
DEFPROP(lastClicks,LastClicksProp)
lastClick :: DynamicProperty w ClickMap LastClicksProp => Int -> Lens' w (Seconds,ClickType,Int,Int)
lastClick t = lastClicks.at t.l'Just (zero,Released,zero,zero)
HASPROP(LastClicksProp,Clickable a,ClickMap,FIELD_LENS(_lastClicks))
#undef IFN_Clickable
#define IFN_Clickable(x...) x

#undef IFN_Hideable
#define IFN_Hideable(x...)
DEFPROP(visible,VisibleProp)
HASPROP(VisibleProp,Hideable a,Bool,FIELD_LENS(_hideableVisible))
#undef IFN_Hideable
#define IFN_Hideable(x...) x

#undef IFN_BoxChild
#define IFN_BoxChild(x...)
#undef IFN_GridChild
#define IFN_GridChild(x...)
DEFPROP(packing,PackingProp)
HASPROP(PackingProp,BoxChild a,BoxPacking,FIELD_LENS(_boxChildPacking))
HASPROP(PackingProp,GridChild a,GridPacking,FIELD_LENS(_gridChildPacking))
#undef IFN_GridChild
#define IFN_GridChild(x...) x
#undef IFN_BoxChild
#define IFN_BoxChild(x...) x
expands :: Lens' BoxPacking Expands
expands = l'1
packAtEnd :: Lens' BoxPacking Bool
packAtEnd = l'2
padding :: Lens' BoxPacking Int
padding = l'3

#undef IFN_KeyboardEnabled
#define IFN_KeyboardEnabled(x...)
DEFPROP(lastKeys,LastKeysProp)
lastKey :: DynamicProperty w KeyMap LastKeysProp => KeyCode -> Lens' w (Seconds,ClickType)
lastKey k = lastKeys.at k.l'Just (zero,Released)
HASPROP(LastKeysProp,KeyboardEnabled a,KeyMap,FIELD_LENS(_lastKeys))
#undef IFN_KeyboardEnabled
#define IFN_KeyboardEnabled(x...) x

#undef IFN_Focusable
#define IFN_Focusable(x...)
DEFPROP(hasFocus,HasFocusProp)
HASPROP(HasFocusProp,Focusable a,Bool,FIELD_LENS(_hasFocus))
#undef IFN_Focusable
#define IFN_Focusable(x...) x

#undef IFN_WProps
#define IFN_WProps(x...)
DEFPROP(gtkHandle,GTKHandleProp)
HASPROP(GTKHandleProp,WProps a,ForeignPtr GtkWidget,FIELD_LENS(_wHandle))
#undef IFN_WProps
#define IFN_WProps(x...) x
