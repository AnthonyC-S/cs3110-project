open Tile
open State
open Textgui

let start_state =
  let blank_state =
    init_state
      [ (1, "Player 1                            Camlkub Tutorial") ]
  in
  let new_player = { (List.hd blank_state.players) with rack = [] } in
  { blank_state with players = [ new_player ] }

let start_tutorial =
  build_board start_state
    (g "  Welcome to the Camlkub Tutorial!"
    ^ "\n\
      \  This tutorial will teach you how to play the game and the \
       commands used.\n\n\
      \  To quit this tutorial at any time, enter \"q\". To continue \
       through the tutorial, press any key.\n")

let tutorial_pages = [ start_tutorial ]
