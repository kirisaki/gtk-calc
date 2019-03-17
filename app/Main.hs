module Main where

import Graphics.UI.Gtk

main :: IO ()
main = do
    initGUI
    xml    <- builderNew
    builderAddFromFile xml "gtk-cal.glade"
    window      <- builderGetObject xml castToWindow "topwindow"
    window `on` objectDestroy $ mainQuit

    widgetShowAll window

    mainGUI

