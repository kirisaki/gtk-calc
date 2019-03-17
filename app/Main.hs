{-# LANGUAGE LambdaCase #-}
module Main where

import Graphics.UI.Gtk
import Control.Monad.IO.Class
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.List as L

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

    buf <- builderGetObject xml castToTextBuffer "display_buffer"
    display <- builderGetObject xml castToTextView "display"
    textBufferSetText buf "0"
    textViewSetBuffer display buf

    q <- atomically newTQueue
    forkIO $ calculator display buf q ["0"]

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


calculator :: (TextViewClass display, TextBufferClass buffer) => display -> buffer ->  TQueue Message -> [String] -> IO ()
calculator d buf q [] = calculator d buf q ["0"]
calculator d buf q st@(x:xs) = atomically (readTQueue q) >>= \m -> do
  let xs' = case m of
        M0 -> (x ++ "0") : xs
        M1 -> (x ++ "1") : xs
        M2 -> (x ++ "2") : xs
        M3 -> (x ++ "3") : xs
        M4 -> (x ++ "4") : xs
        M5 -> (x ++ "5") : xs
        M6 -> (x ++ "6") : xs
        M7 -> (x ++ "7") : xs
        M8 -> (x ++ "8") : xs
        M9 -> (x ++ "9") : xs
        MPlus -> (x ++ "+") : xs
        MMinus -> (x ++ "-") : xs
        MProd -> (x ++ "*") : xs
        MDiv -> (x ++ "/") : xs
        MPoint -> (x ++ ".") : xs
        MEq -> "0" : st
        MC -> case x of
          [_] -> "0" : xs
          _ -> init x : xs
        MAC -> ["0"] ++ xs
  textBufferSetText buf (L.intercalate "\n" (L.reverse xs'))
  textViewSetBuffer d buf
  calculator d buf q xs'
