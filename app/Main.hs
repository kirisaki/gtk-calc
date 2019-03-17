{-# LANGUAGE LambdaCase #-}
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
    entrySetText display "0"

    q <- atomically newTQueue
    forkIO $ calculator display q 0

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


calculator :: EntryClass display => display -> TQueue Message -> Int -> IO ()
calculator d q st = atomically (readTQueue q) >>= \m -> do
  let st' = case m of
        M0 -> st * 10
        M1 -> st * 10 + 1
        M2 -> st * 10 + 2
        M3 -> st * 10 + 3
        M4 -> st * 10 + 4
        M5 -> st * 10 + 5
        M6 -> st * 10 + 6
        M7 -> st * 10 + 7
        M8 -> st * 10 + 8
        M9 -> st * 10 + 9
        MPlus -> undefined
        MMinus -> undefined
        MProd -> undefined
        MDiv -> undefined
        MPoint -> undefined
        MEq -> undefined
        MC -> undefined
        MAC -> undefined
  entrySetText d (show st')
  calculator d q st'
