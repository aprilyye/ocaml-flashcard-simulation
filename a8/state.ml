open Flashcard

(** Represents the abstract type of values representing the study state,
    including an deck [deck], current card [current], thumbs up list
    [thumbs_up], thumbs_down list [thumbs_down], score [score], correct list 
    [correct], and incorrect list [incorrect]. *)
type t = {
  deck: Flashcard.t;
  current: notecard option;
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
let init_state deck=
  {
    deck=deck; 
    current= first_card deck;
    current_face = "terms";
    score = 0;
    high_score = 0;
    correct=[]; 
    prompt = "terms";
    incorrect=[]; 
    mode_deck=deck;
    already_seen = []; 
    starred = [];
    typo = false;
  }

(* the state [st] of the current deck *)
let deck st = 
  st.deck

(* the state [st] of the current card *)
let current_card st =
  st.current

(**[find_next_card deck current] returns a [card] [option] representing 
   the next card after [current] in [deck], and [None] if [current] is the 
   last card in [deck] *)
let rec find_next_card deck current = 
  match deck with 
  | [] -> None
  | h::n::t -> if h = current then Some n else find_next_card (n::t) current
  | h::t -> if h = current && List.length t = 1 then Some (List.hd t) else None

(**[next st] returns the [state] where the next card field has been updated
   to the next card in the deck *) 
let next st  =
  let deck = st.mode_deck in 
  match current_card st with 
  | None -> {st with current = None}
  | Some curr -> 
    ( match deck with 
      | [] ->  {st with current = None}
      | h::n::t -> if h = curr then {st with current = Some n}
        else let m = find_next_card (n::t) curr in {st with current = m}
      | h::t -> if h = curr && List.length t = 1 then 
          {st with current = Some (List.hd t)} else {st with current = None} )

(**[find_prev_card deck current] returns a [card] [option] representing 
   the previous card before [current] in [deck], and [None] if [current] is the 
   first card in [deck] *)
let rec find_prev_card deck current = 
  match deck with 
  | [] -> None
  | h::n::t -> if n = current then Some h else find_prev_card (n::t) current
  | h::t -> if List.length t = 1 && (List.hd t) = current then Some h else None

(**[prev st] returns the [state] where the current card field has been updated
   to the previous card in the deck *) 
let prev st = 
  let deck = st.mode_deck in
  match current_card st with
  | None -> {st with current = None}
  | Some curr -> 
    ( match deck with
      | [] -> {st with current = None}
      | h::n::t -> if n = curr then {st with current = Some h}
        else let m = find_prev_card (n::t) curr in {st with current = m}
      | h::t -> if List.length t = 1 && List.hd t = curr then
          {st with current = Some h} else {st with current = None})

(**[shuffle_deck deck] returns a [deck] with its [notecards] in a random order*)
let shuffle_deck deck =
  match deck with
  | [] -> deck
  | d -> let split_deck = List.partition (fun x -> Random.self_init (); 
                                           Random.bool ()) deck in 
    (List.rev (fst split_deck)) @ (List.rev (snd split_deck))

(**[remove_option st] returns a the value of the [current] card of [st] rather
   than an [option] and fails with "no current card" if [current] is None.*)
let remove_option st = 
  match current_card st with 
  | None -> failwith "no current card"
  | Some card -> card

(**[randomize st] returns a new state that is the same as [st] 
   except with a shuffled [deck]*)
let randomize st = 
  let deck = shuffle_deck st.mode_deck in
  {st with mode_deck = deck; current = Flashcard.first_card deck}

(**[update_score st] updates the score of [st] once the user gets 
   the [current] card correct*)
let update_score st =
  let cur_card = remove_option st in
  match cur_card.attempts with
  | 0 -> st.score + 10
  | 1 -> st.score + 5
  | 2 -> st.score + 2
  | _ -> st.score

(**[change_attempts_of_card card deck] increments the attempts of [card] in 
   [deck] after it is attempted.*)
let rec change_attempts_of_card card deck =
  match deck with
  | [] -> []
  | h::t -> if Flashcard.equals h card then 
      let new_card = {h with attempts = h.attempts+1} in 
      new_card::(change_attempts_of_card card t) else
      h::(change_attempts_of_card card t)

(**[change_attempts st] increments the number of attempts of the [current] card
   in [st]*)
let change_attempts st =
  let cur_card = remove_option st in
  change_attempts_of_card cur_card st.incorrect

(**[get_face st] returns which face ([term] or [defintion]) is supposed to be 
   facing in the current card of [st], and fails if card is None*)
let get_face st = 
  match st.current with 
  | None -> failwith "no current card"
  | Some card -> card.facing 

(**[in_correct_pile st] is true if the [current] card is in the correct
   pile and false otherwise, and fails if card is None*)
let in_correct_pile st = 
  match st.current with 
  | None -> failwith "no current card"
  | Some card -> if Flashcard.mem card st.correct then true else false

(**[star_card st] is a new deck of starred cards with the current card added to it
   if the current card is already starred then the unchanged starred deck is returned *)
let star_card st = 
  let cur_card = remove_option st in 
  let already_in = Flashcard.mem  cur_card st.starred in
  if already_in then st.starred else (st.starred @ [cur_card])


(**[unstar_card st] is a new deck of starred cards with the current card removed from it
   if the current card is not starred then the unchanged starred deck is returned *)
let unstar_card st = 
  let cur_card = remove_option st in 
  let already_in = Flashcard.mem cur_card st.starred in 
  if already_in then Flashcard.remove cur_card st.starred else st.starred

(** [updated_attempts st] adjusts the # wrong attempts for each card in 
    [st.deck] to match the # wrong attempts for each card in [st.incorrect]. *)
let update_attempts st = 
  let rec update_state lst = 
    match lst with 
    | [] -> []
    | h::t -> (match (find_card_opt st.incorrect h) with 
        | None -> h::(update_state t)
        | Some card -> let sum = card.attempts in 
          {h with attempts = sum}::(update_state t))
  in let new_deck = update_state st.deck in 
  {st with deck = new_deck}

(**[pretty_write_list lst deck f] pretty-writes [lst] to [f]. If [deck] is true, 
    then # of times wrong is also written to [f]. *)
let rec pretty_write_list lst deck f =
  match lst with 
  |[] -> ()
  |h::t -> if deck then 
      let attempts = string_of_int h.attempts in 
      output_string f (h.front^","^h.back^","^attempts^"\n")
    else 
      output_string f (h.front^","^h.back^"\n"); 
    pretty_write_list t deck f

(**[write_csv st] prints a summary of stats about [st] to a csv file called
    summary.csv.This includes the list of cards in [st.deck], [st.incorrect], 
    and [st.starred], as well as the % incorrect and % starred. 
    Requires: a file name summary.csv exists in the working directory. *) 
let write_csv st = 
  let file = open_out "summary.csv" in 
  output_string file "SUMMARY\n\n";
  output_string file "TERM, DEFINITION, TIMES WRONG\n";
  pretty_write_list st.deck true file; 
  output_string file "\nINCORRECT\n"; 
  if (List.length st.incorrect) = 0 then output_string file "None\n" else
    (let incorr = (List.length st.incorrect) in let total = (List.length st.deck) in
     output_string file (string_of_int (incorr*100/total)^"% incorrect\n");
     pretty_write_list st.incorrect false file);
  output_string file "\nSTARRED\n"; 
  if (List.length st.starred) = 0 then output_string file "None\n" else 
    (let star = (List.length st.starred) in let total = (List.length st.deck) in
     output_string file (string_of_int (star*100/total) ^"% starred\n");
     pretty_write_list st.starred false file);
  close_out file