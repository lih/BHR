{-# LANGUAGE CPP, ImpredicativeTypes #-}
module Graphics.Widget.Internals (
  -- * Dynamic properties
  module IO.Dynamic,module Graphics.Widget.Traits,module Graphics.Widget.Properties,

  -- * The FFI modules
  module Foreign.C,module Foreign.Ptr,module Foreign.ForeignPtr,
  module Foreign.Marshal.Alloc, module Foreign.Marshal.Array,
  module Foreign.Storable,

  -- * Phantom types for Ptrs
  GClosure,GtkAccelGroup,GtkApplication,GdkEvent,

  -- * Creating GTK Objects
  GObject(..),newGObject,newWidget,

  -- * Type conversion functions
  fromCInt,withCStrings,withGtkWidget,

  -- * General GTK utilities
  connectSignal,parseAccelerator,

  -- * Callback wrappers
  callback__,
  callback_p,
  callback_p_,
  callback_piip_,
  callback_pip_,
  callback_pp_,
  callback_ppb,
  callback_ppiib,
  callback_ppip_,
  callback_ppipp_,
  callback_pppb,

  -- * Trait wrappers
  clickable,hideable,focusable,keyboardEnabled,boxChild,gridChild
  ) where

import Definitive
import qualified Prelude as Prelude
import IO.Dynamic
import Graphics.Widget.Traits
import Graphics.Widget.Properties
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import IO.Time (currentTime)

#include "definitive-graphics.h"

instance Semigroup CInt
instance Monoid CInt
instance Disjonctive CInt where negate = Prelude.negate ; (-) = (Prelude.-)

DEFCALLBACK(_,IO ())
DEFCALLBACK(p,IO (Ptr a))
DEFCALLBACK(p_,Ptr a -> IO ())
DEFCALLBACK(piip_,Ptr a -> CInt -> CInt -> Ptr b -> IO ())
DEFCALLBACK(pip_,Ptr a -> CInt -> Ptr b -> IO ())
DEFCALLBACK(pp_,Ptr a -> Ptr b -> IO ())
DEFCALLBACK(ppb,Ptr a -> Ptr b -> IO Bool)
DEFCALLBACK(ppiib,Ptr a -> Ptr b -> CInt -> CInt -> IO Bool)
DEFCALLBACK(ppip_,Ptr a -> Ptr b -> CInt -> Ptr c -> IO ())
DEFCALLBACK(ppipp_,Ptr a -> Ptr b -> CInt -> Ptr c -> Ptr d -> IO ())
DEFCALLBACK(pppb,Ptr a -> Ptr b -> Ptr c -> IO Bool)

class GObject o where
  g_object_ref :: Ptr o -> IO (Ptr o)
  g_object_ref = g_object_ref_impl
  g_object_unref :: FunPtr (Ptr o -> IO ())
  g_object_unref = g_object_unref_impl
foreign import ccall "g_object_ref" g_object_ref_impl :: Ptr a -> IO (Ptr a)
foreign import ccall "&g_object_unref" g_object_unref_impl :: FunPtr (Ptr a -> IO ())
instance GObject GtkWidget

fromCInt :: CInt -> Int
fromCInt = fromIntegral
withCStrings [] m = m []
withCStrings (s:ss) m = withCString s $ \ps -> withCStrings ss $ \pss -> m (ps:pss)

newGObject :: GObject o => IO (Ptr o) -> IO (ForeignPtr o)
newGObject m = do
  p <- g_object_ref =<< m
  newForeignPtr g_object_unref p
newWidget :: ForeignPtr GtkWidget -> w -> IO (Widget w)
newWidget p w = newDynamic (WProps p w)

withGtkWidget :: Widget a -> (Ptr GtkWidget -> IO b) -> IO b
withGtkWidget w k = getDynamic w gtkHandle >>= \p -> withForeignPtr p k

foreign import ccall "definitive_gtkConnectSignal"
  gtk_connect_signal :: Ptr a -> Ptr CChar -> FunPtr b -> Ptr c -> IO ()
connectSignal :: GObject o => ForeignPtr o -> String -> FunPtr a -> IO ()
connectSignal w s h = withForeignPtr w $ \pw -> withCString s $ \s' -> gtk_connect_signal pw s' h nullPtr

FOREIGN(gtk_accelerator_parse,Ptr CChar -> Ptr CInt -> Ptr CInt -> IO ())
parseAccelerator :: String -> IO (CInt,CInt)
parseAccelerator s = withCString s $ \s' -> do
  alloca $ \pkey -> alloca $ \pmod -> do
    gtk_accelerator_parse s' pkey pmod
    liftA2 (,) (peek pkey) (peek pmod)

data GtkApplication
data GtkAccelGroup
data GdkEvent
data GClosure

instance GObject GtkApplication
instance GObject GtkAccelGroup
  
foreign import ccall "definitive_gdkEvent_get_x"
  gdkEvent_getX :: Ptr GdkEvent -> IO CDouble
foreign import ccall "definitive_gdkEvent_get_y"
  gdkEvent_getY :: Ptr GdkEvent -> IO CDouble
foreign import ccall "definitive_gdkEvent_get_button"
  gdkEvent_getButton :: Ptr GdkEvent -> IO CInt
foreign import ccall "definitive_gdkEvent_get_keyval"
  gdkEvent_getKeyVal :: Ptr GdkEvent -> IO CInt

wTrait :: Trait t => (a -> t a) -> IO (Widget a) -> IO (Widget (t a))
wTrait mk = lensTrait (map mk) (applying traitValue)

FOREIGN(gtk_widget_add_events,Ptr GtkWidget -> CInt -> IO ())
clickable :: IO (Widget a) -> IO (Widget (Clickable a))
clickable mw = do
  ret <- wTrait (Clickable zero) mw
  h <- getDynamic ret gtkHandle
  let addEvent e tp = connectSignal h e =<< callback_pppb $ \_ e _ -> do
        x <- gdkEvent_getX e
        y <- gdkEvent_getY e
        btn <- fromIntegral <$> gdkEvent_getButton e
        tm <- currentTime
        setDynamicQual (Just "gtk") ret (lastClick btn) (tm,tp,floor x,floor y)
        return False
  withForeignPtr h $ \p -> traverse_ (gtk_widget_add_events p) [GDK_BUTTON_PRESS_MASK,GDK_BUTTON_RELEASE_MASK]
  addEvent "button-press-event" Pressed
  addEvent "button-release-event" Released
  return ret

FOREIGN(gtk_widget_grab_focus,Ptr GtkWidget -> IO ())
focusable :: IO (Widget a) -> IO (Widget (Focusable a))
focusable mw = do
  ret <- wTrait (Focusable zero) mw
  h <- getDynamic ret gtkHandle
  let addEvent e tp = do
        connectSignal h e =<< callback_pppb $ \_ _ _ -> False <$ setDynamicQual (Just "gtk") ret hasFocus tp
  addEvent "focus-in-event" True
  addEvent "focus-out-event" False
  return ret <| do onChangeQual (Just "gtk") hasFocus $ \_ f -> when f (withForeignPtr h gtk_widget_grab_focus)

keyboardEnabled :: IO (Widget a) -> IO (Widget (KeyboardEnabled a))
keyboardEnabled mw = do
  ret <- wTrait (KeyboardEnabled zero) mw
  h <- getDynamic ret gtkHandle
  let addEvent e tp = do
        connectSignal h e =<< callback_pppb $ \_ e _ -> False <$ do
          k <- toEnum . fromIntegral <$> gdkEvent_getKeyVal e
          tm <- currentTime
          setDynamicQual (Just "gtk") ret (lastKey k) (tm,tp)
  addEvent "key-press-event" Pressed
  addEvent "key-release-event" Released
  return ret

FOREIGN(gtk_widget_show_all,Ptr GtkWidget -> IO ())
FOREIGN(gtk_widget_hide,Ptr GtkWidget -> IO ())
hideable :: IO (Widget a) -> IO (Widget (Hideable a))
hideable mw = do
  ret <- wTrait (Hideable False) mw
  h <- getDynamic ret gtkHandle
  return ret <| onChange visible (\_ v' -> withForeignPtr h (if v' then gtk_widget_show_all else gtk_widget_hide))

boxChild :: IO (Widget a) -> IO (Widget (BoxChild a))
boxChild mw = wTrait (BoxChild (Expanding,False,0)) mw

gridChild :: (Int,Int) -> IO (Widget a) -> IO (Widget (GridChild a))
gridChild (x,y) mw = wTrait (GridChild (x,y,1,1)) mw
