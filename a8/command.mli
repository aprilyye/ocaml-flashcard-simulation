(** user can go to next flash card by typing command Next, and showing
    answer of current card by command Flash *)
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

(** Raised when an empty command is parsed. *)
exception Empty 

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The rest of the words, if any, become the object phrase.
    Examples: 
    - [parse "    next"] is [Next]
    - [parse "quit"] is [Quit]. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. A command
    is {i malformed} if the verb is neither "quit" nor "next",
    or if the verb is "quit" or "next" and there is a non-empty object phrase*)
val parse : string -> command

(**[remove_space lst] removes space elements from a [lst] of strings 
   corresponding to a [command]*)
val remove_space: string list -> string list