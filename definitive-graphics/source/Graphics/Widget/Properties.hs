{-# LANGUAGE CPP, UndecidableInstances #-}
module Graphics.Widget.Properties where

import Definitive
import IO.Dynamic
import Graphics.Widget.Traits

#include "definitive-graphics.h"

DEFPROP(children,ChildrenProp)
DEFPROP(child,ChildProp)
DEFPROP(child0,Child0Prop)
DEFPROP(child1,Child1Prop)
DEFPROP(child2,Child2Prop)
DEFPROP(child3,Child3Prop)
DEFPROP(label,LabelProp)
DEFPROP(lastTextChange,LastTextChangeProp)
DEFPROP(cursor,CursorProp)
DEFPROP(inputText,InputTextProp)
DEFPROP(selected,SelectedProp)
DEFPROP(alternatives,AlternativesProp)
DEFPROP(expanded,ExpandedProp)
DEFPROP(buttonImage,ButtonImageProp)
DEFPROP(lastActivation,LastActivationProp)
DEFPROP(shortcuts,ShortcutsProp)
DEFPROP(running,RunningProp)
DEFPROP(terminalStatus,TerminalStatusProp)
DEFPROP(lastFrame,LastFrameProp)
DEFPROP(adjustments,AdjustmentsProp)
DEFPROP(viewport,ViewportProp)
