--{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.UI.Gtk
import Control.Monad.IO.Class
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async

data Message
  = M0
  | M1
  | M2
  | M3
  | M4
  | M5
  | M6
  | M7
  | M8
  | M9
  | MPlus
  | MMinus
  | MProd
  | MDiv
  | MPoint
  | MC
  | MAC
  deriving (Eq, Show)

main :: IO ()
main = do
    initGUI

    xml    <- builderNew
    builderAddFromFile xml "gtk-calc.glade"
    window      <- builderGetObject xml castToWindow "topwindow"
    window `on` objectDestroy $ mainQuit

    display <- builderGetObject xml castToEntry "display"
    entrySetText display "aaa"

    q <- atomically newTQueue
    forkIO $ calculator display q

    button0 <- builderGetObject xml castToButton "button_0"
    button0 `on` buttonPressEvent $ tryEvent $ do
      liftIO $ putStrLn "nyaan"

    widgetShowAll window

    mainGUI

calculator :: EntryClass display => display -> TQueue Message  -> IO ()
calculator d q = threadDelay 1000000 >> entrySetText d "fuga"
