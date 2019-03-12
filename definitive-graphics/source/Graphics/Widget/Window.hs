{-# LANGUAGE CPP #-}
module Graphics.Widget.Window(
  Window,window,Dialog,dialog,FileChooser,fileChooser
  ) where

import Definitive
import Graphics.Widget.Internals
import Graphics.Application
import Graphics.Widget.Containers.Internal
import Data.IORef
import IO.Time (currentTime)

#include "definitive-graphics.h"

data Window t = Window {
  _windowHidden :: Bool,
  _windowTitle :: String,
  _windowChild :: Widget t
  }
HASPROP(VisibleProp,Window a,Bool,FIELD_LENS(_windowHidden).iso not not)
HASPROP(ChildProp,Window t,Widget t,FIELD_LENS(_windowChild))
HASPROP(LabelProp,Window a,String,FIELD_LENS(_windowTitle))

FOREIGN(gtk_application_window_new,Ptr GtkApplication -> IO (Ptr GtkWidget))
FOREIGN(gtk_window_set_title,Ptr GtkWidget -> Ptr CChar -> IO ())
FOREIGN(gtk_window_add_accel_group,Ptr GtkWidget -> Ptr GtkAccelGroup -> IO ())
FOREIGN(gtk_widget_hide,Ptr GtkWidget -> IO ())
window :: (?application :: Dynamic Application) => String -> Widget w -> IO (Widget (Window w))
window title w = mfix $ \ret -> do
  app <- getDynamic ?application id
  h <- newGObject (withAppPtr app gtk_application_window_new)
  withForeignPtr h $ \p -> do
    withGtkWidget w (gtk_container_add p)
    withCString title (gtk_window_set_title p)
    withAccels app (gtk_window_add_accel_group p)
    gtk_widget_show_all p
    
  let setVisible v' = withForeignPtr h (if v' then gtk_widget_show_all else gtk_widget_hide)
      
  newWidget h (Window False title w) <| do
    onChangeQual (Just "gtk") child $ \c c' -> withForeignPtr h $ \p -> do
      withGtkWidget c (gtk_container_remove p)
      withGtkWidget c' (gtk_container_add p)
      setVisible =<< getDynamic ret visible
    onChangeQual (Just "gtk") visible $ \_ -> setVisible

data Dialog res a = Dialog {
  _dialogLastActivation :: (Seconds,res),
  _dialogChild :: Widget a,
  _dialogIsVisible :: Bool
  }
HASPROP(VisibleProp,Dialog res a,Bool,FIELD_LENS(_dialogIsVisible))
HASPROP(ChildProp,Dialog res a,Widget a,FIELD_LENS(_dialogChild))
HASPROP(LastActivationProp,Dialog res a,(Seconds,res),FIELD_LENS(_dialogLastActivation))

FOREIGN(gtk_dialog_new_with_buttons,Ptr CChar -> Ptr GtkWidget -> CInt -> Ptr a -> IO (Ptr GtkWidget))
FOREIGN(gtk_dialog_add_button,Ptr GtkWidget -> Ptr CChar -> CInt -> IO ())
FOREIGN(gtk_dialog_get_content_area,Ptr GtkWidget -> IO (Ptr GtkWidget))
dialog :: Widget (Window w) -> String -> [(String,res)] -> Widget a -> IO (Widget (Dialog res a))
dialog parent title buttons w = do
  let resMap = c'map $ fromAList resList
      resList = zip [1..] buttons
  h <- newGObject $ withCString title $ \pt -> do
    p <- withGtkWidget parent $ \parentp -> gtk_dialog_new_with_buttons pt parentp 0 nullPtr
    p' <- gtk_dialog_get_content_area p
    withGtkWidget w (gtk_container_add p')
    for_ resList $ \(i,(b,_)) -> withCString b $ \pb -> gtk_dialog_add_button p pb i
    return p
  ret <- newWidget h (Dialog (0,snd $ head buttons) w False) <| do
    onChangeQual (Just "gtk") visible $ \_ v -> withForeignPtr h $ do
      if v then gtk_widget_show_all else gtk_widget_hide
    onChangeQual (Just "gtk") child $ \c c' -> withForeignPtr h $ \p -> do
      p' <- gtk_dialog_get_content_area p
      withGtkWidget c (gtk_container_remove p')
      withGtkWidget c' (gtk_container_add p')
  connectSignal h "response" =<< callback_pip_ $ \_ n _ -> do
    let result = fromMaybe (error "Invalid result from dialog") $ lookup n resMap
    setDynamicQual (Just "gtk") ret lastActivation . (,snd result) =<< currentTime
  return ret

data GtkFileFilter
instance GObject GtkFileFilter
data GtkFileFilterInfo
instance GObject GtkFileFilterInfo
data GList

data FileChooser = FileChooser {
  _fdSelectedFiles :: [String],
  _fdLastActivation :: Seconds
  }
HASPROP(LastActivationProp,FileChooser,Seconds,FIELD_LENS(_fdLastActivation))
HASPROP(SelectedProp,FileChooser,[String],FIELD_LENS(_fdSelectedFiles))

FOREIGN(gtk_file_chooser_widget_new,CInt -> IO (Ptr GtkWidget))
FOREIGN(gtk_file_filter_new,IO (Ptr GtkFileFilter))
FOREIGN(gtk_file_filter_add_custom,Ptr GtkFileFilter -> CInt -> FunPtr a -> Ptr b -> FunPtr (Ptr b -> IO ()) -> IO ())
FOREIGN(gtk_file_chooser_set_filter,Ptr GtkWidget -> Ptr GtkFileFilter -> IO ())
FOREIGN(gtk_file_chooser_get_filenames,Ptr GtkWidget -> IO (Ptr GList))
FOREIGN(definitive_forGList,FunPtr (IO (Ptr GList)) -> FunPtr (Ptr a -> IO ()) -> IO ())
foreign import ccall "definitive_fileFilterInfo_get_filename"
  fileInfo_peekFilename :: Ptr GtkFileFilterInfo -> IO (Ptr CChar)
fileChooser :: (String -> IO Bool) -> IO (Widget FileChooser)
fileChooser pred = do
  h <- newGObject (gtk_file_chooser_widget_new GTK_FILE_CHOOSER_ACTION_OPEN)
  hff <- newGObject gtk_file_filter_new
  withForeignPtr h $ \p -> withForeignPtr hff $ \pff -> do
    k <- callback_ppb $ \f _ -> do
      s <- peekCString =<< fileInfo_peekFilename f
      pred s
    gtk_file_filter_add_custom pff GTK_FILE_FILTER_FILENAME k nullPtr nullFunPtr
    gtk_file_chooser_set_filter p pff
  ret <- newWidget h (FileChooser [] 0)
  connectSignal h "file-activated" =<< callback_pp_ $ \_ _ -> do
    setDynamicQual (Just "gtk") ret lastActivation =<< currentTime
  connectSignal h "selection-changed" =<< callback_pp_ $ \_ _ -> do
    withForeignPtr h $ \p -> do
      sel <- newIORef []
      mkl <- callback_p (gtk_file_chooser_get_filenames p)
      cb <- callback_p_ $ \d -> peekCString d >>= modifyIORef sel . (:)
      definitive_forGList mkl cb
      setDynamicQual (Just "gtk") ret selected =<< readIORef sel
  return ret
  
