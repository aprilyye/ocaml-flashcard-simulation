open OUnit2
open Flashcard
open Command
open State


let deck_3110 = parse_csv "3110small.csv"

(* card not in deck_3110 *)
let rand_card = 
  {
    front="hi";
    back="there";
    fuzzy = [];
    facing = "front";
    attempts = 0;
  }

(* option version of first card in deck_3110 *)
let first_card_3110_opt = Some {
    front="syntax";
    back="how code is written";
    fuzzy = [];
    facing = "front";
    attempts = 0;
  }

(* semantics flashcard from deck_3110*)
let semantics_card = {
  front="semantics";
  back="how code is *evaluated*";
  fuzzy = ["evaluated"];
  facing = "front";
  attempts = 0;
}

(* semantics flashcard from deck_3110 with attempts = 1 *)
let semantics_card_1= {
  front="semantics";
  back="how code is *evaluated*";
  fuzzy = ["evaluated"];
  facing = "front";
  attempts = 1;
}

(* first card in deck_3110 *)
let first_card_3110 = {
  front="syntax";
  back="how code is written";
  fuzzy = [];
  facing = "front";
  attempts = 0;
}

(* first card in deck_3110 with attemps = 1*)
let first_card_3110_1 = {
  front="syntax";
  back="how code is written";
  fuzzy = [];
  facing = "front";
  attempts = 1;
}

(* test Flashcard.parse_csv *)
let parse_csv_tests = 
  [
    "CSV test 0" >:: (fun _ -> 
        assert_equal (first_card deck_3110) first_card_3110_opt);
    "CSV test 1" >:: (fun _ -> 
        assert_equal (List.length deck_3110) 5);
  ]

(* test flashcard.ml *)
let flashcard_tests = 
  [
    "Flashcard test 0" >:: (fun _ -> 
        assert_equal (term first_card_3110 deck_3110) "syntax");
    "Flashcard test 1" >:: (fun _ -> 
        assert_equal (definition first_card_3110 deck_3110) 
          "how code is written");
    "Flashcard test 2" >:: (fun _ -> 
        assert_equal (find_card deck_3110 first_card_3110) first_card_3110);
    "Flashcard test 3" >:: (fun _ -> 
        assert_equal (fuzzy_set semantics_card deck_3110) ["evaluated"]);
    "Flashcard test 4" >:: (fun _ -> 
        assert_equal (which_fuzzy semantics_card deck_3110 "hi evaluated blah blah" "terms" false) true);
    "Flashcard test 5" >:: (fun _ -> 
        assert_equal (which_fuzzy semantics_card deck_3110 "how code is eval" "terms" false) false);
    "Flashcard test 6" >:: (fun _ -> 
        assert_equal (equals semantics_card first_card_3110) false);
    "Flashcard test 7" >:: (fun _ -> 
        assert_equal (equals semantics_card semantics_card_1) true);
    "Flashcard test 8" >:: (fun _ -> 
        assert_equal (remove semantics_card [semantics_card]) []);
    "Flashcard test 9" >:: (fun _ -> 
        assert_equal (remove semantics_card [first_card_3110]) [first_card_3110]);
    "Flashcard test 10" >:: (fun _ ->
        assert_equal (mem semantics_card [semantics_card_1; first_card_3110]) true);
    "Flashcard test 11" >:: (fun _ ->
        assert_equal (mem semantics_card [first_card_3110]) false);
    "Flashcard test 12" >:: (fun _ ->
        assert_equal (mem semantics_card []) false);
    "Flashcard test 13" >:: (fun _ -> 
        assert_equal (first_card deck_3110) first_card_3110_opt);
    "Flashcard test 14" >:: (fun _ -> 
        assert_equal (first_card []) None);
    "Flashcard test 15" >:: (fun _ -> 
        assert_raises (Failure "card not in deck")
          (fun () -> find_card deck_3110 rand_card));
    "Flashcard test 16" >:: (fun _ -> 
        assert_equal (which_fuzzy semantics_card deck_3110 "evalauted" "terms" true) true);
    "Flashcard test 17" >:: (fun _ -> 
        assert_equal (which_fuzzy semantics_card deck_3110 "synxat" "defs" true) false);
    "Flashcard test 18" >:: (fun _ -> 
        assert_equal 
          (which_fuzzy semantics_card deck_3110 "how code is eavluated" "terms" true) true);
    "Flashcard test 19" >:: (fun _ -> 
        assert_equal 
          (which_fuzzy semantics_card deck_3110 "evalauted other words" "terms" true) true); 
    "Flashcard test 20" >:: (fun _ -> 
        assert_equal 
          (which_fuzzy semantics_card deck_3110 "how code is evaluated" "terms" false) true);
    "Flashcard test 21" >:: (fun _ -> 
        assert_equal 
          (which_fuzzy semantics_card deck_3110 "evaluated other words" "terms" false) true); 

  ] 

(* variables to test Command.remove_space *)
let str1 = "hi     "
let trim_split = String.split_on_char ' ' (String.trim str1)
let str2 = "   hi     there    "
let trim_split_2 = String.split_on_char ' ' (String.trim str2)

(* test command.ml *)
let command_tests = 
  [
    "Command test 0" >:: (fun _ -> 
        assert_equal (remove_space ["h";"i";""]) ["h";"i"]);
    "Command test 1" >:: (fun _ -> 
        assert_equal (remove_space trim_split) ["hi"]);
    "Command test 2" >:: (fun _ -> 
        assert_equal (remove_space trim_split_2) ["hi";"there"]);
    "Command test 3" >:: (fun _ -> 
        assert_equal (parse " flip   ") Flip);
    "Command test 4" >:: (fun _ -> 
        assert_equal (parse "     yes") Random);
    "Command test 5" >:: (fun _ -> 
        assert_equal (parse "p") Practice);
    "Command test 6" >:: (fun _ -> 
        assert_equal (parse "back    ") Back);
  ]

(* initialized state with deck_3110 *)
let state_3110 = {
  deck=deck_3110; 
  current= first_card_3110_opt;
  current_face = "terms"; 
  score = 0;
  high_score = 0; 
  prompt = "terms";
  correct=[]; 
  incorrect=[]; 
  mode_deck = deck_3110;
  already_seen = []; 
  typo = false;
  starred = []
}

(* "next card state" with deck_3110 *)
let state_3110_next = {
  deck=deck_3110; 
  current= Some {
      front="immutable";
      back="state is not changed";
      fuzzy = []; 
      facing = "front"; 
      attempts = 0
    };
  current_face = "terms"; 
  score = 0;
  high_score = 0;
  prompt = "terms";
  mode_deck = deck_3110;
  already_seen = [];
  correct=[]; 
  incorrect=[]; 
  typo = false;
  starred = []
}

(* initialized state with deck_3110  with one card in "incorrect" pile *)
let state_3110_incor_pile = {
  deck=deck_3110; 
  current= first_card_3110_opt;
  current_face = "terms";
  score = 0;
  high_score = 0; 
  prompt = "terms";
  correct=[]; 
  incorrect= [first_card_3110]; 
  mode_deck = deck_3110;
  already_seen = []; 
  typo = false;
  starred = []
}

(* state with deck_3110 with no current card *)
let state_3110_none_curr = {
  deck=deck_3110; 
  current= None;
  current_face = "terms";
  score = 0;
  high_score = 0; 
  prompt = "terms";
  correct=[]; 
  incorrect=[]; 
  mode_deck = deck_3110;
  already_seen = []; 
  typo = false;
  starred = []
}

(* initialized state with deck_3110  with card in "correct" pile *)
let state_3110_corr_pile = {
  deck=deck_3110; 
  current= first_card_3110_opt;
  current_face = "terms";
  score = 0;
  high_score = 0; 
  prompt = "terms";
  correct=[first_card_3110]; 
  incorrect=[]; 
  mode_deck = deck_3110;
  already_seen = []; 
  typo = false;
  starred = []
}

let state_3110_starred_pile = {
  deck=deck_3110; 
  current= first_card_3110_opt;
  current_face = "terms";
  score = 0;
  high_score = 0; 
  prompt = "terms";
  correct=[first_card_3110]; 
  incorrect=[]; 
  mode_deck = deck_3110;
  already_seen = []; 
  typo = false;
  starred = [first_card_3110]
}

(* test state.ml *)
let state_tests = 
  [
    "State test 0" >:: (fun _ -> 
        assert_equal (init_state deck_3110) state_3110);
    "State test 1" >:: (fun _ -> 
        assert_equal (deck state_3110) deck_3110);
    "State test 2" >:: (fun _ -> 
        assert_equal (current_card state_3110) first_card_3110_opt);
    "State test 3" >:: (fun _ -> 
        assert_equal (next state_3110) state_3110_next);
    "State test 4" >:: (fun _ -> 
        assert_equal (List.length (shuffle_deck deck_3110)) 
          (List.length deck_3110));
    "State test 5" >:: (fun _ -> 
        assert_equal (List.length (deck (randomize state_3110)))
          (List.length (deck state_3110))); 
    "State test 6" >:: (fun _ -> 
        assert_equal (List.length (deck (next state_3110)))
          (List.length (deck state_3110_next)));
    "State test 7" >:: (fun _ -> 
        assert_equal (remove_option state_3110) first_card_3110);
    "State test 8" >:: (fun _ -> 
        assert_equal (update_score state_3110) 10);
    "State test 9" >:: (fun _ -> 
        assert_equal (change_attempts state_3110_incor_pile) [first_card_3110_1]);
    "State test 10" >:: (fun _ -> 
        assert_equal (get_face state_3110) "front");
    "State test 11" >:: (fun _ -> 
        assert_raises (Failure "no current card") 
          (fun () -> get_face state_3110_none_curr));
    "State test 12" >:: (fun _ -> 
        assert_raises (Failure "no current card")
          (fun () -> in_correct_pile state_3110_none_curr));
    "State test 13" >:: (fun _ -> 
        assert_equal (in_correct_pile state_3110_corr_pile) true);
    "State test 14" >:: (fun _ -> 
        assert_equal (star_card state_3110) [first_card_3110]);
    "State test 15" >:: (fun _ -> 
        assert_equal (unstar_card state_3110_starred_pile) []);
    "State test 16" >:: (fun _ -> 
        assert_equal (prev state_3110_next) state_3110);  
  ] 

let suite =
  "study app test suite"  >::: List.flatten [
    parse_csv_tests;
    flashcard_tests;
    command_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite