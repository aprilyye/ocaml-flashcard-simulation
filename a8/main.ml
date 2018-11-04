open State 
open Flashcard  
open Command


(**[unknown_user_input] outputs a message if the user inputs something 
   other than a [command]*)
let unknown_user_input () = 
  (ANSITerminal.(print_string [red]
                   "\nI don't know what you are trying to say.\n"))

(**[print_terms st] prints the term of the current [notecard] in st 
   and a message if the player has reached the end of the [deck]*)
let print_terms st = 
  let deck = st.mode_deck in 
  let card = State.current_card st in
  match card with
  | None -> 
    (ANSITerminal.(print_string [red] 
                     "\nCongrats! You are done with this deck.\n");
     print_endline "\n"; Pervasives.exit 0)
  | Some c -> let term = Flashcard.term c deck in
    ANSITerminal.(print_string [white;on_black] ("\n \n" ^ term ^ "\n"));
    print_string "\n"

(**[print_stars_bolded str] prints starred terms in [notecard] in bold*)
let rec print_stars_bolded str =
  let stars_lst = star_index_finder str [] in 
  match List.rev stars_lst with 
  |[] -> ANSITerminal.(print_string [black;on_white] (str ^"\n")); 
    print_string "\n"
  |h::n::t -> if h = 0 
    then let bold = String.sub str 1 (n-1) in 
      ANSITerminal.(print_string [black;on_white;Bold] bold);
      if n = ((String.length str) - 1) then 
        (ANSITerminal.(print_string [black;on_white] "\n"); print_string "\n")
      else 
        print_stars_bolded (String.sub str (n+1) ((String.length str) - (n+1)))
    else 
      let not_bold = String.sub str 0 h in
      let bold = String.sub str (h+1) (n-h-1) in 
      ANSITerminal.(print_string [black;on_white] not_bold);
      ANSITerminal.(print_string [black;on_white;Bold] bold);
      if n = ((String.length str) - 1) 
      then (ANSITerminal.(print_string [black;on_white] "\n"); 
            print_string "\n")
      else 
        print_stars_bolded (String.sub str (n+1) ((String.length str) - (n+1)))
  |h::[] -> failwith "invalid starring"

(**[print_back st] prints the [definition] of the notecard in the current state 
   [st], and prints a message if the player has reached the end of the [deck]*)
let print_defs st = 
  let deck = st.mode_deck in 
  match State.current_card st with 
  | None -> 
    (ANSITerminal.(print_string [red] 
                     "\nCongrats! You are done with this deck.\n");
     print_endline "\n"; Pervasives.exit 0)
  | Some card -> 
    let definition = Flashcard.definition card deck in 
    ANSITerminal.(print_string [black;on_white] ("\n \n"));
    print_stars_bolded definition

let rec pp_list lst deck = 
match lst with 
| [] -> ()
| h::t -> if deck then 
let attempts = h.attempts in 
ANSITerminal.(print_string [red] ("\nWrong attempts: " ^string_of_int attempts));
else (); 
ANSITerminal.(print_string [white;on_black] ("\n" ^ h.front^"\n"));
ANSITerminal.(print_string [black;on_white] ("\n"));
print_stars_bolded h.back; pp_list t deck

let print_stats st = 
ANSITerminal.erase ANSITerminal.Above;
ANSITerminal.(print_string [Bold] "-------------------------------------------------------------------------------\n");
ANSITerminal.(print_string [Bold] ("DECK\n"));
pp_list st.deck true;
ANSITerminal.(print_string [Bold] ("\n\nINCORRECT\n"));
if (List.length st.incorrect) = 0 then print_string "None\n" else
(let incorr = (List.length st.incorrect) in let total = (List.length st.deck) in 
ANSITerminal.(print_string [red] (string_of_int (incorr*100/total)^"% incorrect\n"));
pp_list st.incorrect false);
ANSITerminal.(print_string [Bold] ("\n\nSTARRED\n"));
if (List.length st.starred) = 0 then print_string "None\n" else 
(let star = (List.length st.starred) in let total = (List.length st.deck) in 
ANSITerminal.(print_string [red] (string_of_int (star*100/total) ^"% starred\n"));
pp_list st.starred false);
ANSITerminal.(print_string [Bold] "-------------------------------------------------------------------------------\n")



(**[user_input] reads user input and parses the commands using [Command.parse]; 
   if the input is not a [command], an exception calls [unknown_user_input].
   This user_input is used for every command*)
let rec user_input () =
  ANSITerminal.(print_string [default] ("\n"));
  print_string  "> ";
  match read_line () with
  | exception exn -> unknown_user_input (); user_input ()
  | cmd -> ( match Command.parse cmd with 
      | exception exn -> unknown_user_input (); user_input ()
      | _ -> Command.parse cmd )

(**[user_input_test] is similar to  [user_input] but only used while the user 
   is in [test_mode]*)
let rec user_input_test () = 
  ANSITerminal.(print_string [default] ("\n"));
  print_string "> ";
  match read_line () with 
  | exception exn -> unknown_user_input (); user_input_test ();
  | cmd -> ( match Command.parse cmd with 
      | exception exn -> Text cmd
      | Quit -> Quit
      | Next -> Next
      | Reset -> Reset 
      | _ -> unknown_user_input (); user_input_test ())

let user_input_text () = 
  ANSITerminal.(print_string [default] ("\n"));
  print_string "> ";
  read_line ()

(**[prompt_shuffle()] prompts the user to choose to practice or test with the 
   [deck] in the order of the input CSV, or in a random order*)
let rec prompt_shuffle () = 
  ANSITerminal.(print_string [red] "\nShuffle the deck?"); 
  print_string "\n[yes]\n[no]\n";
  let cmd = user_input () in 
  match cmd with 
  | Random -> ANSITerminal.erase ANSITerminal.Above; true
  | Ordered -> ANSITerminal.erase ANSITerminal.Above; false 
  | Quit -> ANSITerminal.erase ANSITerminal.Above; 
    (ANSITerminal.(print_string [red] 
                     "\nHope you feel more prepared after studying, you got this!\n");
     print_endline "\n"; Pervasives.exit 0)
  | _ -> 
    ANSITerminal.erase ANSITerminal.Above; unknown_user_input (); 
    prompt_shuffle ()

(**[make_card ()] is a new flashcard with term and definition based on the user input*)
let make_card ()  = 
  ANSITerminal.(print_string [red] "\nTerm:"); 
  let front = user_input_text() in 
  ANSITerminal.(print_string [red] "\nDefinition:"); 
  let back = user_input_text () in 
  let fuzzy = Flashcard.make_fuzzy_set back (List.rev (Flashcard.star_index_finder back [])) in 
  {front = front; back = back; fuzzy = fuzzy; facing = 
                                                "front"; attempts = 0}

let rec prompt_terms () = 
  ANSITerminal.(print_string [red] "\nDo you want to be prompted with terms or definitions?"); 
  print_string "\n[terms]\n[defs]\n";
  let cmd = user_input () in 
  match cmd with 
  | Terms -> ANSITerminal.erase ANSITerminal.Above; true
  | Defs -> ANSITerminal.erase ANSITerminal.Above; false
  | Quit -> ANSITerminal.erase ANSITerminal.Above; 
    (ANSITerminal.(print_string [red] 
                     "\nHope you feel more prepared after studying, you got this!\n");
     print_endline "\n"; Pervasives.exit 0) 
  | _ -> ANSITerminal.erase ANSITerminal.Above; unknown_user_input (); 
    prompt_terms ()

let rec prompt_typo () = 
  ANSITerminal.(print_string [red] "\nDo you want to allow typos?"); 
  print_string "\n[yt] yes typos\n[nt] no typos\n";
  let cmd = user_input() in 
  match cmd with 
  | Typo b -> ANSITerminal.erase ANSITerminal.Above; b
  | Quit -> ANSITerminal.erase ANSITerminal.Above; 
    (ANSITerminal.(print_string [red] 
                     "\nHope you feel more prepared after studying, you got this!\n");
     print_endline "\n"; Pervasives.exit 0) 
  | _ -> ANSITerminal.erase ANSITerminal.Above; unknown_user_input (); 
    prompt_typo ()



(**[choose_mode st] prompts the user to select one of
   five modes and proceeds with that mode *)
let rec choose_mode st =
  (* let st = update_attempts st in  *)
  ANSITerminal.(print_string [red] "\nWhat do you want to do next?");
  print_string "\n[p] practice all\n[pw] practice wrongs\n[ps] practice starred\n[t] test all\n[tw] test wrongs\n[ts] test starred\n[ac] add card\n[stats]\n[quit]\n";
  let cmd = user_input () in 
  match cmd with 
  | Quit -> ANSITerminal.erase ANSITerminal.Above; 
    (ANSITerminal.(print_string [red] 
                     "\nHope you feel more prepared after studying, you got this!\n");
     print_endline "\n"; Pervasives.exit 0)
  | Practice -> ANSITerminal.erase ANSITerminal.Above;
    start_practice_mode {st with mode_deck=st.deck;incorrect=[]; score = 0; 
                                 current = (Flashcard.first_card st.deck); 
                                 already_seen=[]} "p"
  | Practice_wrongs -> ANSITerminal.erase ANSITerminal.Above;
    start_practice_mode {st with mode_deck=st.incorrect; score = 0; 
                                 current = (Flashcard.first_card st.incorrect); 
                                 already_seen=[]} "pw"
  | Test_wrongs -> ANSITerminal.erase ANSITerminal.Above;
    start_test_mode {st with mode_deck=st.incorrect; 
                             current = (Flashcard.first_card st.incorrect); 
                             already_seen=[]} "tw"
  | Test -> ANSITerminal.erase ANSITerminal.Above;
    start_test_mode {st with mode_deck=st.deck; score = 0; incorrect=[]; current = (Flashcard.first_card st.deck); already_seen=[]} "t"
  | Practice_starred -> start_practice_mode {st with mode_deck=st.starred; score = 0; incorrect=[]; current = (Flashcard.first_card st.starred); already_seen=[]} "ts"
  | Test_starred -> start_test_mode {st with mode_deck=st.starred; score = 0; incorrect=[]; current = (Flashcard.first_card st.starred); already_seen=[]} "ts"
  | Add_card -> let new_card = make_card () in choose_mode ({st with deck = (if Flashcard.mem new_card st.deck then st.deck
                                                                             else st.deck @ [new_card])})
  | Stats -> print_stats st; choose_mode st                                                                     
  | _ -> ANSITerminal.erase ANSITerminal.Above; unknown_user_input (); choose_mode st


(**[next_turn_test st mode] executes the user's command upon testing through
   the deck of flashcards once*)
and next_turn_test st mode = 
  let cmd = user_input_test () in
  match cmd with 
  (*we dont update high score in quit because they are quiting so it doesnt 
    matter*)
  | Quit -> ANSITerminal.erase ANSITerminal.Above;
    let is_high = st.score > st.high_score in 
    if is_high then (ANSITerminal.(print_string [red] 
                                     ("\n\nCongrats! New high score of " ^
                                      (string_of_int st.score) ^ ".\n"));
                     print_endline "\n"; Pervasives.exit 0) 
    else (ANSITerminal.(print_string [red] 
                          ("\n\nYou finished with a score of "
                           ^ (string_of_int st.score) ^
                           ". This is not a high score.\n"));
          print_endline "\n"; Pervasives.exit 0)
  | Next ->
    let got_correct = in_correct_pile st in
    let card_no_option = remove_option st in 
    let already_seen = Flashcard.mem card_no_option st.already_seen in 
    let _ =  if got_correct then (ANSITerminal.erase ANSITerminal.Above)
        else if already_seen then (ANSITerminal.erase ANSITerminal.Above) else
        let correct_ans = if st.prompt = "terms" then card_no_option.back else
            card_no_option.front in if st.prompt = "defs" then 
          (ANSITerminal.(print_string [white;on_black] 
                           ("\n \nCorrect answer: " ^ correct_ans ^ "\n")); 
           print_string "\n")
        else 
          ((ANSITerminal.(print_string [black;on_white]
                            ("\n \nCorrect answer: "))); 
           print_stars_bolded correct_ans) in 
    (let next = State.next st in 
     let next = if got_correct 
       then {next with 
             current_face = next.prompt; 
             already_seen = (remove_option st::st.already_seen)} 
       else if already_seen then next else {next with current_face = next.prompt; 
                       already_seen = (remove_option st::st.already_seen); 
                       incorrect = if 
                         Flashcard.mem card_no_option next.incorrect 
                         then change_attempts st 
                         else 
                           (next.incorrect @ 
                            [{card_no_option with attempts = 
                                                    card_no_option.attempts +1; }])} in 
     let next = update_attempts next in 
     match State.current_card next with 
     | None -> let is_high = st.score > st.high_score in 
       if is_high then 
         (ANSITerminal.(print_string [red] 
                          ("\n\nCongrats! New high score of " ^
                           (string_of_int st.score) ^ ".\n"));
          choose_mode next)
       else (ANSITerminal.(print_string [red] 
                             ("\n\nYou finished with a score of "
                              ^ (string_of_int st.score) ^
                              ". This is not a high score.\n"));
             choose_mode next)
     | Some card -> if next.prompt = "terms" then 
         (print_terms next ; next_turn_test next mode) else 
         print_defs next; next_turn_test next mode)
  | Text t -> 
    ( let deck = st.mode_deck in 
      let is_correct_ans = 
        Flashcard.which_fuzzy (remove_option st) deck t st.prompt st.typo in 
      if is_correct_ans 
      then (ANSITerminal.erase ANSITerminal.Above;
      (ANSITerminal.(print_string [green] ("\nCorrect!\n")));
            ( 
              let new_correct = st.correct @ [remove_option st] in 
              let new_incorrect = 
                Flashcard.remove (remove_option st) st.incorrect in  
              let next = {st with score = (update_score st); 
                                  correct = new_correct; 
                                  incorrect = new_incorrect } in
              let next = update_attempts next in
              next_turn_test next mode))
      else 
        let correct_ans = if st.prompt = "terms" then (remove_option st).back else
            (remove_option st).front in 
        ANSITerminal.(print_string [red] "\n\nIncorrect");
        if st.prompt = "defs" then 
          (ANSITerminal.(print_string [white;on_black] 
                           ("\n \nCorrect answer: " ^ correct_ans^ "\n")); print_string "\n")
        else 
          ((ANSITerminal.(print_string [black;on_white]
                            ("\n \nCorrect answer: "))); 
           print_stars_bolded correct_ans);
        let new_incorrect = if Flashcard.mem (remove_option st) st.incorrect 
          then change_attempts st
          else st.incorrect @ [{(remove_option st) with 
                                attempts = (remove_option st).attempts+1}] in
        let next = {st with incorrect = new_incorrect; 
                            already_seen = (remove_option st::st.already_seen)} 
        in let next = update_attempts next 
        in next_turn_test next mode )
  |Reset -> ANSITerminal.erase ANSITerminal.Above; choose_mode st
  | _ -> ANSITerminal.erase ANSITerminal.Above;unknown_user_input (); 
    next_turn_test st mode 


(**[start_test_mode st mode] starts the testing mode in either randomized
   flashcard order or not*)
and start_test_mode st mode = 
  let rand = prompt_shuffle () in 
  let terms = prompt_terms () in 
  let typo = prompt_typo () in 
  match rand with 
  | true -> ANSITerminal.erase ANSITerminal.Above;
    let st = (if terms = true 
              then {st with current_face = "terms"; prompt = "terms"; typo = typo} else 
                {st with current_face = "defs"; prompt = "defs"; typo = typo }) in 
    (let rand_state = State.randomize st in 
     let deck = rand_state.mode_deck in 
     match  Flashcard.first_card deck with 
     | None -> ANSITerminal.(print_string [red] 
                               "\nThere are no cards in this deck."); 
       choose_mode st
     | Some card -> (if st.prompt = "terms" then 
                       print_terms rand_state else print_defs rand_state);
       next_turn_test rand_state mode)
  | false -> ANSITerminal.erase ANSITerminal.Above;
    let st = (if terms = true 
              then {st with current_face = "terms"; prompt = "terms"; typo = typo} else 
                {st with current_face = "defs"; prompt = "defs"; typo = typo }) in 
    (let deck = st.mode_deck in 
     match  Flashcard.first_card deck with 
     | None -> ANSITerminal.(print_string [red] 
                               "\nThere are no cards in this deck."); 
       choose_mode st
     | Some card -> (if st.prompt = "terms" 
                     then print_terms st else print_defs st);
       next_turn_test st mode) 

(**[next_turn_practice st] prints output corresponding to the users [command], 
   and prompts the user to enter either [practice] or [test] mode at the 
   beginning/end of the deck and calls [start_practice_mode] or 
   [start_test_mode]
   (to be implemented in next sprint)*)
and next_turn_practice st mode = 
  let cmd = user_input () in
  match cmd with 
  | Quit -> ANSITerminal.erase ANSITerminal.Above; 
    (ANSITerminal.(print_string [red] 
                     "\nHope you feel more prepared after studying, you got this!\n");
     print_endline "\n"; Pervasives.exit 0)
  | Flip -> ANSITerminal.erase ANSITerminal.Above; 
    (if st.current_face = "terms" 
     then (print_defs st; 
           next_turn_practice ({st with current_face ="defs"}) mode )
     else (print_terms st; 
           next_turn_practice ({st with current_face = "terms"}) mode ))
  | Next -> ANSITerminal.erase ANSITerminal.Above;
    (let next = State.next st in 
     let next = {next with current_face = next.prompt} in 
     match State.current_card next with 
     | None -> choose_mode st
     | Some n -> if next.prompt = "terms" 
       then (print_terms next ; 
             next_turn_practice next mode) 
       else 
         print_defs next; next_turn_practice next mode)
  | Star -> let new_starred = State.star_card st in 
    ANSITerminal.(print_string [red] "Card starred \n");
    (next_turn_practice ({st with starred = new_starred}) mode)
  | Unstar -> (let new_starred = State.unstar_card st in 
               ANSITerminal.(print_string [red] "Card unstarred \n");
               (next_turn_practice ({st with starred = new_starred}) mode))
  | Reset -> ANSITerminal.erase ANSITerminal.Above;choose_mode st
  | Back -> ANSITerminal.erase ANSITerminal.Above;
    (
      let prev = State.prev st in 
      let prev = {prev with current_face = prev.prompt} in 
      match State.current_card prev with
      | None -> choose_mode st
      | Some p -> if prev.prompt = "terms"
        then (print_terms prev; next_turn_practice prev mode)
        else 
          print_defs prev; next_turn_practice prev mode
    )
  | _ -> ANSITerminal.erase ANSITerminal.Above; unknown_user_input (); next_turn_practice st mode


(**[start_practice_mode state] starts the game with a shuffled or ordered 
   [deck] depending on user input and prints the [front] of the 
   first [notecard] to start the game*)
and start_practice_mode st mode= 
  let rand = prompt_shuffle () in 
  let terms = prompt_terms () in 
  match rand with 
  | true -> ANSITerminal.erase ANSITerminal.Above; 
    let st = (if terms = true 
              then {st with current_face = "terms"; prompt = "terms"} else 
                {st with current_face = "defs"; prompt = "defs" }) in 
    (let rand_state = State.randomize st in 
     let deck = rand_state.mode_deck in 
     match  Flashcard.first_card deck with 
     | None -> ANSITerminal.(print_string [red] 
                               "\nThere are no cards in this deck."); 
       choose_mode st
     | Some card -> (if st.prompt = "terms" then 
                       print_terms rand_state else print_defs rand_state);
       next_turn_practice rand_state mode)
  | false -> ANSITerminal.erase ANSITerminal.Above; 
    let st = (if terms = true 
              then {st with current_face = "terms"; prompt = "terms"} else 
                {st with current_face = "defs"; prompt = "defs" }) in 
    (let deck = st.mode_deck in 
     match  Flashcard.first_card deck with 
     | None -> ANSITerminal.(print_string [red] 
                               "\nThere are no cards in this deck."); 
       choose_mode st
     | Some card -> (if st.prompt = "terms" 
                     then print_terms st else print_defs st);
       next_turn_practice st mode)

(** [start_study f] starts the study in file [f]. *)
let rec start_study deck =
  let cmd = user_input () in 
  match cmd with 
  | Quit -> ANSITerminal.erase ANSITerminal.Above;
    (ANSITerminal.(print_string [red] 
                     "\nHope you feel more prepared after studying, you got this!\n");
     print_endline "\n"; Pervasives.exit 0)
  | Practice -> ANSITerminal.erase ANSITerminal.Above; 
    let st = State.init_state deck in start_practice_mode st "p"
  | Test ->  ANSITerminal.erase ANSITerminal.Above; 
    let st = State.init_state deck in start_test_mode st "t"
  | Add_card -> let new_card = make_card () in
  (ANSITerminal.(print_string [red]
                                "\nWhat would you like to do next?"));
    print_string "\n[p] practice\n[t] test\n[ac] add card\n";
    start_study (if Flashcard.mem new_card deck then deck
                 else deck @ [new_card])
  | _ -> (unknown_user_input (); (ANSITerminal.(print_string [red]
    "\nWhat would you like to do next?"));
    print_string "\n[p] practice\n[t] test\n[ac] add card\n";
    start_study deck)

(**[main_helper file_input] will continuously prompt the user to enter a valid
   csv if the user inputs an invalid csv. Otherwise, if the user inputs a valid
   csv, the program will parse the csv and start the study session. *)
let rec main_helper file_input =
  match file_input with
  | exception exn -> ANSITerminal.erase ANSITerminal.Above; Pervasives.exit 0
  | file_name -> ANSITerminal.erase ANSITerminal.Above; 
    (let file = parse_csv file_name in 
     match file with
     | [] ->  (ANSITerminal.(print_string [red]
                               "\nNot a valid CSV. Please enter another CSV.\n\n"));
       print_string  "> ";
       main_helper (read_line ())
     | deck -> (ANSITerminal.(print_string [red]
                                "\nWhat would you like to do?"));
       print_string "\n[p] practice\n[t] test\n[ac] add card\n";
       start_study deck)

(**[main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.erase ANSITerminal.Above;
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to your study sesh!\n");
  print_endline "Enter the name of the csv you want to study.\n";
  print_string  "> ";
  main_helper (read_line ())

let _ = main ()