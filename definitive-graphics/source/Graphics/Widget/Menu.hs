{-# LANGUAGE CPP #-}
module Graphics.Widget.Menu (
  MenuBar,Menu,MenuItem,menuBar,menu,menuItem,accelMenuItem,
  ) where

import Definitive
import Graphics.Widget.Internals
import Graphics.Widget.Containers.Internal
import Graphics.Application
import Graphics.Widget.Label
import IO.Time (currentTime)

#include "definitive-graphics.h"

data MenuItem a = MenuItem {
  _itemChild :: Widget a,
  _itemLastActivation :: Seconds,
  _itemShortcutsID :: Maybe String,
  _itemSubMenu :: Maybe (Widget (Menu a))
  }
HASPROP(ChildrenProp,MenuItem a,Maybe (Widget (Menu a)),FIELD_LENS(_itemSubMenu))
HASPROP(ChildProp,MenuItem a,Widget a,FIELD_LENS(_itemChild))
HASPROP(LastActivationProp,MenuItem a,Seconds,FIELD_LENS(_itemLastActivation))
HASPROP(ShortcutsProp,MenuItem a,Maybe String,FIELD_LENS(_itemShortcutsID))

HASPROP(ChildrenProp,[Widget a],[Widget a],id)
newtype MenuBar a = MenuBar { _menuBarMenus :: [Widget (MenuItem a)] }
HASPROP(ChildrenProp,MenuBar a,[Widget (MenuItem a)],FIELD_LENS(_menuBarMenus))
data Menu a = Menu { _menuIsVisible :: Bool, _menuItems :: [Widget (MenuItem a)] }
HASPROP(ChildrenProp,Menu a,[Widget (MenuItem a)],FIELD_LENS(_menuItems))
HASPROP(VisibleProp,Menu a,Bool,FIELD_LENS(_menuIsVisible))

lookupPath path = find (\(_,Assoc k _) -> k==path) . by ascList

FOREIGN(gtk_menu_item_new,IO (Ptr GtkWidget))
FOREIGN(gtk_menu_item_set_submenu,Ptr GtkWidget -> Ptr GtkWidget -> IO ())
FOREIGN(gtk_widget_add_accelerator,Ptr GtkWidget -> Ptr CChar -> Ptr GtkAccelGroup -> CInt -> CInt -> CInt -> IO ())
menuItem :: (?application :: Dynamic Application) => Widget a -> IO (Widget (MenuItem a))
menuItem sub = mdo
  h <- newGObject $ do
    p <- gtk_menu_item_new
    withGtkWidget sub (gtk_container_add p)
    return p
  ret <- newWidget h (MenuItem sub 0 Nothing Nothing) <| do
    onChangeQual (Just "gtk") children $ \_ sub' -> withForeignPtr h $ \p -> do
      gtk_menu_item_set_submenu p nullPtr
      for_ sub' $ \w -> withGtkWidget w (gtk_menu_item_set_submenu p)
    onChangeQual (Just "gtk") child $ \w w' -> withForeignPtr h $ \p -> do
      withGtkWidget w (gtk_container_remove p)
      withGtkWidget w' (gtk_container_add p)
      gtk_widget_show_all p
    onChangeQual (Just "gtk") shortcuts $ \_ mpath -> for_ mpath $ \path -> do
      mshort <- lookupPath path <$> getDynamic ?application shortcuts
      app <- getDynamic ?application id
      for_ mshort $ \(k,_) -> do
        (key,mods) <- parseAccelerator k
        withCString "activate" $ \pact -> withForeignPtr h $ \p -> do
          withAccels app $ \paccg -> gtk_widget_add_accelerator p pact paccg key mods GTK_ACCEL_VISIBLE
    onChange lastActivation $ \_ _ -> do
      mp <- getDynamic ret shortcuts
      for_ mp $ \path -> do
        m <- getDynamic ?application shortcuts
        for_ (lookupPath path m) $ \(_,Assoc _ m) -> m
  connectSignal h "activate" =<< callback_pp_ $ \_ _ -> do
    setDynamicQual (Just "gtk") ret lastActivation =<< currentTime
  return ret
  
accelMenuItem :: (?application :: Dynamic Application) => String -> IO (Widget (MenuItem AccelText))
accelMenuItem s = do
  txt <- accelText s
  menuItem txt <| do
    onChange shortcuts $ \_ sh -> do
      m <- getDynamic ?application shortcuts
      setDynamic txt (label.l'2) (sh >>= \x -> m^.getter (lookupPath x).applying l'1) 

FOREIGN(gtk_menu_new,IO (Ptr GtkWidget))
FOREIGN(gtk_menu_bar_new,IO (Ptr GtkWidget))
menuCons :: Orientation -> [Widget (MenuItem a)] -> IO (Widget [Widget (MenuItem a)])
menuCons o l = do
  h <- newGObject (case o of Horizontal -> gtk_menu_bar_new; Vertical -> gtk_menu_new)
  withForeignPtr h $ \p -> do
    for_ l (\w -> withGtkWidget w (gtk_container_add p))
    gtk_widget_show_all p
  newWidget h l <| do
    onChange children $ \c c' -> withForeignPtr h $ \p -> do
      for_ c $ \w -> withGtkWidget w (gtk_container_remove p)
      for_ c' $ \w -> withGtkWidget w (gtk_container_remove p)
      gtk_widget_show_all p
menuBar :: [Widget (MenuItem a)] -> IO (Widget (MenuBar a))
menuBar l = isoTrait (mapping (iso _menuBarMenus MenuBar)) (menuCons Horizontal l)  
FOREIGN(gtk_menu_set_accel_group,Ptr GtkWidget -> Ptr GtkAccelGroup -> IO ())
FOREIGN(gtk_menu_popup,Ptr GtkWidget -> Ptr GtkWidget -> Ptr GtkWidget -> FunPtr a -> Ptr b -> CInt -> CUInt -> IO ())
FOREIGN(gtk_get_current_event_time,IO CUInt)
menu :: (?application :: Dynamic Application) => [Widget (MenuItem a)] -> IO (Widget (Menu a))
menu l = mdo
  ret <- lensTrait (map (Menu False)) (applying children) (menuCons Vertical l) <| do
    onChangeQual (Just "gtk") visible $ \_ v -> when v $ withGtkWidget ret $ \p -> do
      gtk_menu_popup p nullPtr nullPtr nullFunPtr nullPtr 1 =<< gtk_get_current_event_time
  h <- getDynamic ret gtkHandle
  withForeignPtr h $ \p -> do
    app <- getDynamic ?application id
    withAccels app (gtk_menu_set_accel_group p)
  connectSignal h "hide" =<< callback_pp_ $ \_ _ -> setDynamicQual (Just "gtk") ret visible False
  return ret



