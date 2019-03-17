module Main where

import Graphics.UI.Gtk
import Control.Monad.IO.Class
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

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
  | MEq
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

    mapM_ (\(s, m) -> buildButton xml s m q)
      [ ("0", M0)
      , ("1", M1)
      , ("2", M2)
      , ("3", M3)
      , ("4", M4)
      , ("5", M5)
      , ("6", M6)
      , ("7", M7)
      , ("8", M8)
      , ("9", M9)
      , ("plus", MPlus)
      , ("minus", MMinus)
      , ("prod", MProd)
      , ("div", MDiv)
      , ("point", MPoint)
      , ("eq", MEq)
      , ("c", MC)
      , ("ac", MAC)
      ]

    widgetShowAll window

    mainGUI

buildButton ::  Builder -> String -> Message -> TQueue Message -> IO (ConnectId Button)
buildButton b s m q =
  builderGetObject b castToButton ("button_" ++ s) >>=
  (\x -> x `on` buttonPressEvent $ tryEvent $ liftIO (atomically $ writeTQueue q m))


calculator :: EntryClass display => display -> TQueue Message  -> IO ()
calculator d q = forever $ print =<< atomically (readTQueue q)
