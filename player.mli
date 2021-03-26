type p

(* type player = { name : string ; number : int ; played_initial_meld :
   bool ; rack : rack ; start_turn_rack : rack; past_rack : rack list ;
   score : int }

   function make_player : initializes a new player

   type rack = tile list

   function fill_rack : adds a list of 14 tiles by Pile.deal_rack and
   assigns it to player.rack

   function sort_by_color : sorts the rack based on color first then
   ascending numbers, jokers last.

   function sort_by_number : sorts the rack based off number first then
   color, jokers last

   function print_rack : pretty print rack

   function rack_size : returns number of tiles in rack

   (from here-------------)

   exception EmptyRack

   function empty_past_rack : creates a new player object with the
   “past_rack” field set to [ ]

   function update_past_rack : creates a new player object with a new
   rack list with current “rack” field value appended to the “past_rack”
   field.

   function undo_past_rack : creates a new player object with the last
   rack element in the “past_rack” field deleted and set to the “rack”
   field.

   function reset_current_turn_rack : creates a new player object with
   the first rack element in the “past_rack” field and calls
   empty_past_rack *)
