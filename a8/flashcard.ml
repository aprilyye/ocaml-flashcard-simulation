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

(**[count_same w1 w2] is the number of chars in w1 that match up with 
   the char at the same index of [w2] *)
let rec count_same (w1:string) (w2:string) index count= 
  if index >= String.length w1 || index >= String.length w2 then count
  else if (String.get w1 index) = (String.get w2 index) then 
    count_same w1 w2 (index +1) (count+1)
  else count_same w1 w2 (index +1) (count)

(**[word_typo] w1 w2 is true if [w1] equals [w2] taking typos into consideration and false ow
   if [w1] contains 70% or more of the same chars at the same index as [w2] then they are equal
   example [word_typo "abcd" "abtd"] is true because three out of four chars are identical at the same indecies *)
let rec word_typo (w1:string) (w2:string) = 
  let similar = count_same w1 w2 0 0 in 
  let longer = if (String.length w1) > (String.length w2) then (String.length w1)
    else (String.length w2) in 
  let percent_correct = (float_of_int similar) /. (float_of_int longer) in 
  if (compare percent_correct (0.7)) < 0 then false else true

(**[check_typos word_lst1 word_lst2] is true if each elements in [word_lst1] equals
   the corresponding element in [word_lst2] at the same index and false ow*)
let rec check_typos (word_lst1:string list) (word_lst2:string list) = 
  match word_lst1, word_lst2 with 
  | [],[] -> true
  | w1::t1, w2::t2 -> word_typo w1 w2 && check_typos t1 t2
  | w1, [] -> false 
  | [], w2 -> false


(*[find_typo_word w wl] finds word [w] in word list [wl] accounting for typos 
  in [w]*)
let rec find_typo_word w wl = 
  match wl with 
  | []-> false
  | h::t -> if word_typo w h then true else find_typo_word w t

(*[compare_user_fuzzy_yt user_ans fuzzy] returns true if every word/phrase in 
  [fuzzy], accounting for typos, is present in [user_input]*)
let rec compare_user_fuzzy_yt user_ans fuzzy = 
  match fuzzy with 
  |[]-> true
  |h::t -> if find_typo_word h user_ans then compare_user_fuzzy_yt user_ans t 
    else false

(*[compare_user_fuzzy_nt user_ans fuzzy] returns true if every word/phrase in 
  [fuzzy] is present in [user_input]*)
let rec compare_user_fuzzy_nt user_ans fuzzy =
  match fuzzy with
  | [] -> true
  | h::t -> if (contains user_ans h) = false then false else 
      (compare_user_fuzzy_nt user_ans t)

(**[compare_user_fuzzy user_ans typo] checks if the [user_ans] string contains 
   each word in the [fuzzy] set, or required words of the current card; 
   if typos are allowed, then the answer is accepted if each word in the 
   user input is simlar enough to each word in the fuzzy set.*)
let compare_user_fuzzy user_ans fuzzy typo = 
  if typo then compare_user_fuzzy_yt (String.split_on_char ' ' user_ans) fuzzy
  else compare_user_fuzzy_nt user_ans fuzzy 

(**[which_fuzzy card deck user_ans prompt] compares the [user_input] to the 
   [term] of [card] in [deck] if [prompt] is [definiton], and to the [definiton]
   if [prompt] is term. Fails with "empty deck" if the deck is empty and 
   "card not in deck" if [card] is not in [deck]*)
let which_fuzzy card deck user_ans prompt typo=
  match deck with
  | [] -> failwith ("Empty Deck")
  | deck -> if prompt = "defs" then 
      (if typo then check_typos (String.split_on_char ' ' user_ans) (String.split_on_char ' ' (term card deck)) else
         (if (String.compare user_ans (term card deck) = 0) then true else false))
    else (let defn = definition card deck in
          let fuzzy = fuzzy_set card deck in
          match fuzzy with
          | [] -> (if typo then check_typos (String.split_on_char ' ' defn) (String.split_on_char ' ' user_ans) 
                   else if (String.compare defn user_ans) = 0 then true else false)
          | _ -> compare_user_fuzzy user_ans fuzzy typo)

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
    {front = front; back = back; fuzzy = fuzzy; 
     facing = "front"; attempts = 0}::(parse_line file)

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

(**[find_card_opt deck card] returns [Some card] if [card] is present in [deck], 
    and [None] otherwise.  *)
let rec find_card_opt deck card = 
  match deck with 
  | [] -> None
  | h::t -> if equals card h then Some h else find_card_opt t card

