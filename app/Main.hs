{-# LANGUAGE LambdaCase #-}
module Main where

import Graphics.UI.Gtk
import Control.Monad.IO.Class
import Control.Monad
import Control.Concurrent
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.List as L
import Data.Attoparsec.Text as A
import Data.Text
import Control.Applicative

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
calculator d buf' q st@(x:xs) = atomically (readTQueue q) >>= \m -> do
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
        MEq ->
          case feed (parse expP (pack x)) (pack "") of
            Done _ e ->
              "0" : show (eval e) : st
            _ ->
              "0" : "error"  : st
        MC -> case x of
          [_] -> "0" : xs
          _ -> L.init x : xs
        MAC -> ["0"] ++ xs
  idleAdd ( do
              buf <- textViewGetBuffer d
              textBufferSetText buf (L.intercalate "\n" (L.reverse xs'))
              return True
          ) priorityLow
  calculator d buf' q xs'

expP :: Parser Exp
expP =  choice [prodP, divP] <|> choice [plusP, minusP] <|> leafP

leafP :: Parser Exp
leafP = Leaf <$> double

plusP :: Parser Exp
plusP =  OpPlus <$> leafP <* char '+' <*> expP

minusP :: Parser Exp
minusP = OpMinus <$> leafP <* char '-' <*> expP

prodP :: Parser Exp
prodP =  OpProd <$> leafP <* char '*' <*> expP

divP :: Parser Exp
divP =  OpDiv <$> leafP <* char '/' <*> expP

data Exp
  = Leaf Double
  | OpPlus Exp Exp
  | OpMinus Exp Exp
  | OpProd Exp Exp
  | OpDiv Exp Exp
  deriving (Show, Eq)

eval :: Exp -> Exp
eval (Leaf v) = Leaf v
eval (OpPlus (Leaf x) (Leaf y)) = Leaf $ x + y
eval (OpPlus x y) = eval $ OpPlus (eval x) (eval y)
eval (OpMinus (Leaf x) (Leaf y)) = Leaf $ x - y
eval (OpMinus x y) = eval $ OpMinus (eval x) (eval y)
eval (OpProd (Leaf x) (Leaf y)) = Leaf $ x * y
eval (OpProd x y) = eval $ OpProd (eval x) (eval y)
eval (OpDiv (Leaf x) (Leaf y)) = Leaf $ x / y
eval (OpDiv x y) = eval $ OpDiv (eval x) (eval y)
