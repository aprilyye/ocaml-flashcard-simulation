(** The abstract type of values representing the study state. *)
type t = {
  deck: Flashcard.t;
  current: Flashcard.notecard option;
  current_face: string;
  score: int;
  high_score: int;
  prompt: string;
  correct: Flashcard.t;
  incorrect: Flashcard.t;
  mode_deck: Flashcard.t;
  already_seen: Flashcard.t;
  starred: Flashcard.t;
  typo: bool;
}

(** [init_state deck] is the initial state of the game when studying the deck
    [deck]. In that state the user is currently looking at the first card of
    the deck, and all other list trackers are empty. *)
val init_state : Flashcard.t -> t

(** [deck st] is the identifier of the current deck that the user is
    studying. *)
val deck: t-> Flashcard.t

(** [current_card st] is the identifier of the current card that the user is
    studying in the deck [deck]. *)
val current_card: t-> Flashcard.notecard option

(**[next st] returns the [state] where the next card field has been updated
   to the next card in the deck *) 
val next: t -> t

(**[prev st] returns the [state] where the prev card field has been updated
   to the prev card in the deck *) 
val prev: t -> t

(**[shuffle_deck deck] returns a [deck] with its [notecards] in a random order*)
val shuffle_deck: Flashcard.t -> Flashcard.t

(**[randomize st] returns a new state that is the same as [st] 
   except with a shuffled [deck]*)
val randomize: t -> t

(**[remove_option st] returns a the value of the [current] card of [st] rather
   than an [option] and fails if [current] is None.*)
val remove_option: t -> Flashcard.notecard

(**[update_score st] updates the score of [st] once the user gets 
   the [current] card correct*)
val update_score: t -> int

(**[change_attempts st] increments the number of attempts of the [current] card
   in [st]*)
val change_attempts: t -> Flashcard.notecard list

(**[get_face st] returns which face ([term] or [defintion]) is supposed to be 
   facing in the current card of [st]*)
val get_face: t -> string

(**[in_correct_pile st] is true if the [current] card is in the correct
   pile and false otherwise*)
val in_correct_pile: t -> bool

val star_card: t-> Flashcard.t

val unstar_card: t-> Flashcard.t

val update_attempts: t -> t

val write_csv: t -> unit
