{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Functor                       (void)
import qualified Graphics.UI.FLTK.LowLevel.FL       as FL
import           Graphics.UI.FLTK.LowLevel.FLTKHS
import           Graphics.UI.FLTK.LowLevel.Fl_Types

main :: IO ()
main = do
  win <- doubleWindowNew (toSize (640,480)) Nothing (Just "Simple Fl_Text_Editor")
  buff <- textBufferNew Nothing Nothing
  edit <- textEditorNew (toRectangle (20,20,(640-40),(480-40))) Nothing
  setBuffer edit (Just buff)
  showWidget win
  setText buff "%-\n(:def main \"Hello, World!\""
  void $ FL.run
