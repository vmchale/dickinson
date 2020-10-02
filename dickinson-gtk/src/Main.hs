{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Functor                  (void)
import qualified Data.Text                     as T
import           GI.Gtk                        (Label (..), Window (..), windowTitle)
import           GI.Gtk.Declarative            (Attribute ((:=)), bin, notebook, page, widget)
import           GI.Gtk.Declarative.App.Simple (App (..), AppView, Transition, run)

-- https://haskell-at-work.com/episodes/2018-11-13-gtk-programming-with-haskell.html
-- http://book.realworldhaskell.org/read/gui-programming-with-gtk-hs.html
-- https://github.com/owickstrom/gi-gtk-declarative/blob/master/examples/MenuBar.hs

data State = Inp !T.Text
           | None

data Event = ReadText
           | Run

view' :: State -> AppView Window Event
view' _ = bin Window
    [ windowTitle := "Dickinson Interactive Development Environment"
    , #widthRequest := 400
    , #heightRequest := 300
    ] $
        notebook []
            [ page "(new)" $ widget Label [#label := "Work"]
            ]

update' :: State -> Event -> Transition State Event
update' = undefined

main :: IO ()
main = void $ run App
    { view         = view'
    , update       = update'
    , inputs       = []
    , initialState = None
    }
