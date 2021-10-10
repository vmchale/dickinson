{-# LANGUAGE OverloadedStrings #-}

-- from documentation here: https://github.com/deech/fltkhs-demos
module Main (main) where

import qualified Data.ByteString.Lazy             as BSL
import           Data.Functor                     (void)
import           Data.Text.Encoding               (encodeUtf8)
import qualified Graphics.UI.FLTK.LowLevel.FL     as FL
import           Graphics.UI.FLTK.LowLevel.FLTKHS (toPosition, toRectangle, toSize)
import qualified Graphics.UI.FLTK.LowLevel.FLTKHS as FL
import           Language.Dickinson               (pipelineBSLErr)


main :: IO ()
main = do
  window <- FL.doubleWindowNew (toSize (640, 480+140)) Nothing (Just "Dickinson GUI")

  inp <- FL.textBufferNew Nothing Nothing
  out <- FL.textBufferNew Nothing Nothing

  edit <- FL.textEditorNew (toRectangle (20, 20, 640-40, 480-40)) Nothing
  button <- FL.buttonNew (FL.Rectangle (toPosition (20, 480)) (toSize (60, 20))) (Just "Run")

  FL.setBuffer edit (Just inp)
  -- FL.flcSetFont FL.screen (FL.FontSize 14)
  FL.setText inp "%-\n\n(:def main \n  (:oneof\n    (| \"heads\")\n    (| \"tails\")))"
  display <- FL.textDisplayNew (FL.Rectangle (toPosition (20, 480+40)) (toSize (480-40, 80))) Nothing
  FL.setBuffer display (Just out)

  FL.setCallback button (\_ -> wireUp inp out)

  FL.showWidget window

  void FL.run

  where wireUp :: FL.Ref FL.TextBuffer -> FL.Ref FL.TextBuffer -> IO ()
        wireUp inp out = do
                    inText <- FL.getText inp
                    res <- pipelineBSLErr [] "(gui)" (BSL.fromStrict $ encodeUtf8 inText)
                    case res of
                        Right txt -> FL.setText out txt
                        Left err  -> FL.setText out err
