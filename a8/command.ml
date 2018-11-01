(** the type of the command, in which the user can choose their next action, 
    practice mode, and preference of a shuffled deck or not *)
type command =
  | Next
  | Flip
  | Star 
  | Unstar
  | Quit
  | Random
  | Ordered 
  | Practice
  | Test
  | Practice_wrongs
  | Test_wrongs
  | Practice_starred
  | Test_starred
  | Terms
  | Defs
  | Add_card
  | Typo of bool
  | Text of string 
  | Reset 

exception Empty 

(** user input is malformed *)
exception Malformed

(**[remove_space lst] takes in a [lst] corresponding to strings from the parsed 
   command and evaluates to the list with all the space elements removed*) 
let rec remove_space lst = 
  match lst with
  | [] -> []
  | h::t -> if h <> "" then (h) :: remove_space t else remove_space t

(**[parse str] takes in a [string] and  removes spaces, then checks if the 
   inputs are valid. If so, it evaluates to a [command] if the inputs are
   valid- returns [Quit] the game if the input is quit, and [Next] flash card
   term if the command is [Next]. If the command is empty, it [parse] returns 
   [Empty], and if it is invalid, it returns [Malformed]*)
let parse str =
  if String.length str = 0 then raise (Empty) else 
    let trimmed_str = String.trim str in 
    let word_lst = String.split_on_char ' ' trimmed_str in 
    let word_lst_no_space = remove_space word_lst in 
    match word_lst_no_space with
    | [] -> raise Malformed
    | h::t -> if h = "next" then Next
      else if h = "flip" then Flip
      else if h = "quit" then Quit
      else if h = "p" then Practice
      else if h = "t" then Test
      else if h = "pw" then Practice_wrongs
      else if h = "tw" then Test_wrongs
      else if h = "ps" then Practice_starred
      else if h = "ts" then Test_starred
      else if h = "yes" then Random 
      else if h = "no" then Ordered 
      else if h = "terms" then Terms
      else if h = "defs" then Defs
      else if h = "star" then Star
      else if h = "unstar" then Unstar
      else if h = "ac" then Add_card
      else if h = "yt" then Typo true 
      else if h = "nt" then Typo false
      else if h = "reset" then Reset 
      else raise Malformed
