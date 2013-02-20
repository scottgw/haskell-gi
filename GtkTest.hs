module Main where

import Foreign.Ptr
import Foreign.C.String

import qualified Gtk

main = do
  Gtk.init 0 nullPtr
  w <- Gtk.windowNew 0

  Gtk.windowSetTitle w "It worked!!!"

  b <- Gtk.buttonNewWithLabel "Push it (make the beat go harder)"
  Gtk.containerAdd w b

  Gtk.widgetShowAll w
  Gtk.main
