module Data.Input.Button (
  ButtonState,KeyButtonState(Press,Release), Button(..)
  ) where

import Definitive
import Graphics.UI.GLFW

data Button = CharKey Char
            | SpecialKey SpecialKey
            | MouseButton MouseButton
            deriving (Eq,Show)
type ButtonState = KeyButtonState
