-- | This module provides the main functions that allow one to create graphical
-- interfaces (based on GTK+ for the moment).
--
-- It is meant to be easy to use, so that creating windows and
-- reacting to events isn't such a pain. Thus, the minimal code needed
-- to create a working windowed application is the following :
--
-- > import Graphics.Widget
-- > 
-- > main = runApplication $ window "Hello" =<< text "Hello, world !"
--
-- For more complicated interfaces, you only need to know that a
-- widget is a collection of properties (like "label", "lastClick",
-- ...) that can be independently written (with `setDynamic widget
-- property value`), read (with `getProp widget property`), and
-- listened to for changes (with the `onChange` family of functions).
--
-- A more complex example of interface follows, with a button that,
-- when clicked, changes the text of a label (press Ctrl+q to quit) :
--
-- > main = runApplication $ do
-- >   runPropState ?application $ do
-- >     shortcuts =~ insert "Quit" (Assoc "<Ctrl>q" quitApplication)
-- >   lbl <- subwidget (text "Hello, world !")
-- >   btn <- (subwidget.clickable) (button (Just "Click me")) <| do 
-- >     onChange lastClick $ \_ _ -> setDynamic lbl label "Hello, you"
-- >   contents <- box2 Horizontal btn lbl
-- >   win <- window "Example" contents
-- >   setDynamic win visible True

module Graphics.Widget (
  module IO.Dynamic,
  -- * Running applications
  module Graphics.Application,
  -- * Widgets
  module Graphics.Widget.Window,
  module Graphics.Widget.Menu,
  module Graphics.Widget.Box,
  module Graphics.Widget.Grid,
  module Graphics.Widget.Switch,
  module Graphics.Widget.Label,
  module Graphics.Widget.Image,
  module Graphics.Widget.Button,
  module Graphics.Widget.TextInput,
  module Graphics.Widget.ComboBox,
  module Graphics.Widget.Notebook,
  module Graphics.Widget.Expander,
  module Graphics.Widget.Terminal,
  module Graphics.Widget.GLArea,
  module Graphics.Widget.Scrollable,
  module Graphics.Widget.Separator,
  -- * Traits
  module Graphics.Widget.Traits,module Graphics.GDK.KeyCodes,
  clickable,keyboardEnabled,hideable,boxChild,gridChild,focusable,
  -- * Properties
  module Graphics.Widget.Properties
  ) where

import Graphics.GDK.KeyCodes
import IO.Dynamic
import Graphics.Widget.Internals
import Graphics.Widget.Traits
import Graphics.Widget.Properties
import Graphics.Widget.Label
import Graphics.Widget.Button
import Graphics.Widget.Box
import Graphics.Widget.Grid
import Graphics.Widget.TextInput
import Graphics.Widget.ComboBox
import Graphics.Widget.Expander
import Graphics.Widget.Terminal
import Graphics.Widget.Notebook
import Graphics.Widget.Menu
import Graphics.Widget.Window
import Graphics.Widget.Switch
import Graphics.Widget.Image
import Graphics.Widget.GLArea
import Graphics.Widget.Scrollable
import Graphics.Widget.Separator
import Graphics.Application



