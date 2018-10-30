open Unix
open Str

type front = string
type back = string

(** Type [notecard] denotes one notecard in the deck with a [front] (term)s
    and [back] (definition)
    [facing] can either be "front" or "back." *)
type notecard = {
  front: front;
  back: back;
  fuzzy: string list;
  facing: string;
  attempts: int;
}
(**Type [deck] is a [list] of [notecards]*)
type t = notecard list

(**[first_card deck] returns the a [card] [option] representing the first card 
   and fails if the [deck] is empty*)
let first_card (deck:t) = 
  match deck with 
  | [] -> None
  | h::t -> Some (h)

(**[find_card deck card] returns [card] if [card] is in [deck], otherwise 
   fails*)
let find_card deck card = 
  if List.mem card deck then card else failwith "card not in deck"

(**[term card deck] returns the [front] of [card] if [card] is in the [deck]
   and fails if [deck] is empty or [card] is not in [deck]*)
let term card deck =
  match deck with 
  | [] -> ""
  | deck -> let found_card = find_card deck card in
    found_card.front

(**[definition card deck] returns the [back] of [card] if [card] is in the 
   [deck] and fails if [deck] is empty or [card] is not in [deck]*)
let definition card deck =
  match deck with 
  | [] -> ""
  | deck -> let found_card = find_card deck card in
    found_card.back

(**[fuzzy_set card deck] finds [card] [deck] and returns the list that
   that corresponds to the starred terms in the [definiton] or [card]*)
let fuzzy_set card deck = 
  match deck with 
  | [] -> []
  | deck -> let found_card = find_card deck card in found_card.fuzzy

(**[star_index_finder str acc] returns the indices of [*] in [str], which 
   denote where the required words in the [definition] start/end.
 * Requires: [str] has an even number of [*]
 * Example: star_index_finder "*alice bob* sally *mary*" 
   returns [0; 10; 18; 23]*)
let rec star_index_finder str acc =
  match str with
  | "" -> acc
  | s -> match String.index_opt s '*' with
    | None -> acc
    | Some i -> if (List.length acc = 0) then 
        star_index_finder (String.sub s (i+1) ((String.length s) -i-1)) (i::acc)
      else 
        star_index_finder (String.sub s (i+1) ((String.length s) -i-1)) 
          ((i + List.hd acc + 1)::acc)

(**[make_fuzzy_set str index_lst] returns the list of starred terms in 
   the definition ([str]) of a card. 
 *Example: make_fuzzy_set "*alice bob* sally *mary*" [0; 10; 18; 23]
   returns ["alice bob"; "mary"] *)
let rec make_fuzzy_set str index_lst =
  match index_lst with
  | [] -> []
  | h::n::t -> String.sub str (h+1) (n-h-1) :: (make_fuzzy_set str t)
  | _::_ -> failwith ("Invalid Starring")

(**[Contains s1 s2] returns [true] if [s2] is a substring 
   of [s1] and [false] otherwise. This function is sourced from Stack Overflow*)
let contains s1 s2 =
  let re = Str.regexp_string s2 in
  try 
    ignore (Str.search_forward re s1 0); 
    true
  with Not_found -> false

(**[compare_user_fuzzy user_ans] checks if the [user_ans] string contains 
   each word in the [fuzzy] set, or required words of the current card. *)
let rec compare_user_fuzzy user_ans fuzzy =
  match fuzzy with
  | [] -> true
  | h::t -> if (contains user_ans h) = false then false 
    else (compare_user_fuzzy user_ans t)

(**[which_fuzzy card deck user_ans prompt] compares the [user_input] to the 
   [term] of [card] in [deck] if [prompt] is [definiton], and to the [definiton]
   if [prompt] is term. Fails with "empty deck" if the deck is empty and "card not
   in deck" if [card] is not in [deck]*)
let which_fuzzy card deck user_ans prompt =
  match deck with
  | [] -> failwith ("Empty Deck")
  | deck -> if prompt = "defs" then 
      (if (String.compare user_ans (term card deck) = 0) then true else false)
    else (let defn = definition card deck in
          let fuzzy = fuzzy_set card deck in
          match fuzzy with
          | [] -> if (String.compare defn user_ans) = 0 then true else false
          | _ -> compare_user_fuzzy user_ans fuzzy)

(**[parse_line file] returns a [deck] from CSV input [file] and an empty [deck] 
   if the file is empty*)
let rec parse_line file = 
  match input_line file with 
  | exception exn -> []
  | line -> let comma = String.index line ',' in 
    let front = String.sub line 0 comma in 
    let back = String.sub line (comma+1) ((String.length line) - 
                                          (String.length front)-1) in
    let fuzzy = make_fuzzy_set back (List.rev (star_index_finder back [])) in 
    {front = front; back = back; fuzzy = fuzzy; facing = 
                                                  "front"; attempts = 0}::(parse_line file)

(**[parse_csv csv] checks that [csv] is a .csv file and opens the file if it 
   is in the working directory, then parses the lines uses [parse_line]. 
   if [csv] does not have the .csv extension, or is not in the current 
   working directory, then the user is prompted to input another [csv].*)
let parse_csv (csv : string) = 
  if Filename.check_suffix csv ".csv" then 
    match open_in (getcwd () ^ Filename.dir_sep ^ csv) with 
    | exception exn -> []
    | _ -> parse_line (open_in (getcwd () ^ Filename.dir_sep ^ csv))
  else []

(**[equals f1 f2] is true if [f1] equals [f2] and false otherwise
   equality is defined by the front and the back of [f1] and [f2] 
   being identical disregarding the rest of the attributes of [f1] and [f2] *)
let equals f1 f2 =
  if f1.front = f2.front && f1.back = f2.back then true else false

(**[remove card deck] is [deck] with [card] removed from it. if [card] is not in
   [deck] then [deck] is returned *)
let rec remove card deck = 
  match deck with
  | [] -> []
  | h::t -> if equals h card then remove card t else h::(remove card t)

(**[mem card deck] is true if [card] is in [deck] and false otherwise *)
let rec mem card deck = 
  match deck with 
  | [] -> false
  | h::t -> if equals card h then true else mem card t 