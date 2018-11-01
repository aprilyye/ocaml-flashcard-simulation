open Str
type front = string
type back = string

(** Type [notecard] denotes one notecard in the deck that has a term 
    and a definition*)
type notecard = {
  front: front;
  back: back;
  fuzzy: string list;
  facing: string;
  attempts: int
}

(**Type [t] represents a deck of notecards*)
type t = notecard list

(**[first_card deck] returns the a [notecard] [option] representing the first 
   notecard and fails if the [deck] is empty*)
val first_card: t-> notecard option

(**[find card deck] 
   returns [card] if the card is in the [deck] fails the [card] is not in the 
   [deck]*)
val find_card: t -> notecard -> notecard

(**[term card deck] returns the the [term] of the given notecard if the card is 
   in the [deck] and fails if the [deck] is the empty or the [card] is not in 
   the [deck]*)
val term: notecard-> t -> back

(**[definition cd deck] returns the the [term] of the given notecard if the 
   [card] is in the [deck] and fails if the [deck] is  empty or the [notecard] 
   is not in the [deck]*)
val definition: notecard-> t -> back

(**[fuzzy_set card deck] 
   returns the list of starred terms ([fuzzy]) in the [definiton]
   of [card] in [decl]*)
val fuzzy_set: notecard -> t -> string list

(**[parse_csv file] checks that an input [file] is a CSV and opens the [file] if
   it is in the working directory, then parses the lines into a [deck] of 
   [notecards]*)
val parse_csv: string -> t 

(**[which_fuzzy card deck user input prompt] compares the 
   [user input] to either the [term] or [definition]
   of a [notecard] depending on [prompt]*)
val which_fuzzy: notecard -> t -> String.t -> string -> bool -> bool

(**[equals n1 n2] compares two notecards based on their [front] and [back] 
   fields*)
val equals: notecard -> notecard -> bool

(**[remove notecard deck] removes [notecard] from [deck]*)
val remove: notecard -> t -> t

(**[mem notecard deck] returns true if a [notecard] is in a [deck] and false 
   otherwise*)
val mem: notecard -> t -> bool

(**[star_index_finder str] returns a list of the indices of * in [str]*)
val star_index_finder: string -> int list -> int list

val make_fuzzy_set: string -> int list -> string list
