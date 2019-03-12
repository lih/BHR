{-# LANGUAGE CPP #-}
module Graphics.Application ( ShortcutsMap, Application, withAppPtr, withAccels, runApplication, quitApplication ) where

import Definitive
import Graphics.Widget.Internals
import Data.IORef
import Data.Bits ((.&.))

#include "definitive-graphics.h"

type ShortcutsMap = Map String (Assoc String (IO ()))
data Application = Application {
  _applicationPtr :: ForeignPtr GtkApplication,
  _applicationAccels :: ForeignPtr GtkAccelGroup,
  _applicationShortcuts :: ShortcutsMap,
  _applicationRunning :: Bool
  }
HASPROP(ShortcutsProp,Application,ShortcutsMap,FIELD_LENS(_applicationShortcuts))
HASPROP(RunningProp,Application,Bool,FIELD_LENS(_applicationRunning))

withAccels :: Application -> (Ptr GtkAccelGroup -> IO a) -> IO a
withAccels a = withForeignPtr (_applicationAccels a)
withAppPtr :: Application -> (Ptr GtkApplication -> IO a) -> IO a
withAppPtr a = withForeignPtr (_applicationPtr a)

FOREIGN(g_application_quit,Ptr GtkApplication -> IO ())
FOREIGN(g_application_run,Ptr GtkApplication -> CInt -> Ptr (Ptr CChar) -> IO CInt)
FOREIGN(g_cclosure_new,FunPtr a -> Ptr b -> FunPtr c -> IO (Ptr GClosure))
FOREIGN(gtk_accel_group_connect,Ptr GtkAccelGroup -> CInt -> CInt -> CInt -> Ptr GClosure -> IO ())
FOREIGN(gtk_accel_group_disconnect,Ptr GtkAccelGroup -> Ptr GClosure -> IO ())
FOREIGN(gtk_accel_group_new,IO (Ptr GtkAccelGroup))
FOREIGN(gtk_application_new,Ptr CChar -> CInt -> IO (Ptr GtkApplication))
-- | Runs an IO action in the context of a graphical application. This
-- action can create widgets and windows, and the application will
-- terminate after all the widgets thus created are destroyed, or
-- the application's "running" property is set to False.
runApplication :: ((?application :: Dynamic Application) => IO a) -> IO (Maybe a)
runApplication k = do
  h <- newGObject (gtk_application_new nullPtr 0)
  acc <- newGObject gtk_accel_group_new
  res <- newIORef Nothing
  connectSignal h "activate" =<< callback_pp_ $ \_ _ -> do
    app <- newDynamic (Application h acc zero True) <| do
      onChange running $ \_ x -> unless x (withForeignPtr h g_application_quit)
      onChange shortcuts $ \_ map -> do
        withForeignPtr acc $ \pacc -> do
          gtk_accel_group_disconnect pacc nullPtr
          for_ (map^.ascList) $ \(n,Assoc _ m) -> do
              (key,mods) <- parseAccelerator n
              cb <- callback_ppiib $ \_ _ _ mods -> do
                when (mods .&. GDK_RELEASE_MASK == 0) m
                return False
              clos <- g_cclosure_new cb nullPtr nullFunPtr
              gtk_accel_group_connect pacc key mods GTK_ACCEL_VISIBLE clos
    let ?application = app
    writeIORef res . Just =<< k
  withForeignPtr h $ \app -> fromIntegral <$> g_application_run app 0 nullPtr
  readIORef res

-- | Quits the current application
quitApplication :: (?application :: Dynamic Application) => IO ()
quitApplication = setDynamic ?application running False
