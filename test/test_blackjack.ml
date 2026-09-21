open OUnit2
open Blackjack
open Game

(* Helpers *)

let make_card suit rank : Card.card = { Card.suit; rank }
let ace suit = make_card suit Card.Ace
let num suit n = make_card suit (Card.Num n)
let jack suit = make_card suit Card.Jack
let queen suit = make_card suit Card.Queen
let king suit = make_card suit Card.King
let hand cards = List.fold_left (fun h c -> Hand.add_card c h) Hand.empty cards

let string_of_phase = function
  | Player_turn -> "Player_turn"
  | Dealer_turn -> "Dealer_turn"
  | Round_over -> "Round_over"

let string_of_outcome = function
  | Player_win -> "Player_win"
  | Player_blackjack -> "Player_blackjack"
  | Dealer_win -> "Dealer_win"
  | Push -> "Push"

let string_of_outcome_option = function
  | None -> "None"
  | Some o -> "Some " ^ string_of_outcome o

let string_of_list elem_to_string lst =
  "[" ^ String.concat "; " (List.map elem_to_string lst) ^ "]"

let string_of_card_list = string_of_list Card.to_string
let string_of_int_list = string_of_list string_of_int
let string_of_hand_list = string_of_list Hand.to_string
let string_of_outcome_option_list = string_of_list string_of_outcome_option

let string_of_deal_option = function
  | None -> "None"
  | Some (card, rest) ->
      Printf.sprintf "Some (%s, remaining deck: %s)" (Card.to_string card)
        (string_of_card_list (Deck.to_list rest))

let string_of_state st =
  Printf.sprintf
    "{ player=%s; dealer=%s; deck=%s; phase=%s; outcome=%s; \n\
    \     player_balance=%d; dealer_balance=%d; current_bet=%d; active_hand=%d; \n\
    \     hand_bets=%s; hand_outcomes=%s }"
    (Hand.to_string st.player_hand)
    (Hand.to_string st.dealer_hand)
    (string_of_card_list (Deck.to_list st.deck))
    (string_of_phase st.phase)
    (string_of_outcome_option st.outcome)
    st.player_balance st.dealer_balance st.current_bet st.active_hand
    (string_of_int_list st.hand_bets)
    (string_of_outcome_option_list st.hand_outcomes)

let assert_deal_option_equal expected actual =
  assert_equal ~printer:string_of_deal_option expected actual

let assert_card_equal expected actual =
  assert_equal ~printer:Card.to_string expected actual

let assert_card_list_equal expected actual =
  assert_equal ~printer:string_of_card_list expected actual

let assert_int_list_equal expected actual =
  assert_equal ~printer:string_of_int_list expected actual

let assert_hand_list_equal expected actual =
  assert_equal ~printer:string_of_hand_list expected actual

let assert_outcome_option_list_equal expected actual =
  assert_equal ~printer:string_of_outcome_option_list expected actual

let assert_state_equal expected actual =
  assert_equal ~printer:string_of_state expected actual

let assert_int_equal expected actual =
  assert_equal ~printer:string_of_int expected actual

let assert_bool_equal expected actual =
  assert_equal ~printer:string_of_bool expected actual

let assert_string_equal expected actual =
  assert_equal ~printer:(fun s -> s) expected actual

let assert_phase_equal expected actual =
  assert_equal ~printer:string_of_phase expected actual

let assert_outcome_option_equal expected actual =
  assert_equal ~printer:string_of_outcome_option expected actual

let assert_float_equal expected actual =
  assert_bool
    (Printf.sprintf "expected %.6f but got %.6f" expected actual)
    (abs_float (expected -. actual) < 0.000001)

let assert_distribution_equal expected actual =
  assert_int_equal (List.length expected) (List.length actual);
  List.iter2
    (fun (expected_score, expected_probability)
         (actual_score, actual_probability) ->
      assert_int_equal expected_score actual_score;
      assert_float_equal expected_probability actual_probability)
    expected actual

let make_state ?(phase = Game.Player_turn) ?outcome ?(player_balance = 50)
    ?(dealer_balance = 50) ?(current_bet = 1) player_cards dealer_cards
    deck_cards =
  let player_hand = hand player_cards in
  {
    player_hand;
    dealer_hand = hand dealer_cards;
    deck = Deck.of_list deck_cards;
    phase;
    outcome;
    player_balance;
    dealer_balance;
    current_bet;
    player_hands = [ player_hand ];
    active_hand = 0;
    hand_bets = [ current_bet ];
    hand_done = [ false ];
    hand_outcomes = [ None ];
  }

let make_multi_state ?(phase = Game.Player_turn) ?outcome ?(player_balance = 50)
    ?(dealer_balance = 50) ?(active_hand = 0) ?hand_done ?hand_outcomes
    player_card_groups dealer_cards hand_bets deck_cards =
  let player_hands = List.map hand player_card_groups in
  let hand_count = List.length player_hands in
  let actual_done =
    match hand_done with
    | Some d -> d
    | None -> List.map (fun _ -> false) player_hands
  in
  let actual_outcomes =
    match hand_outcomes with
    | Some o -> o
    | None -> List.map (fun _ -> None) player_hands
  in
  if hand_count <> List.length hand_bets then
    invalid_arg "hand_bets length must match player hands";
  if hand_count <> List.length actual_done then
    invalid_arg "hand_done length must match player hands";
  if hand_count <> List.length actual_outcomes then
    invalid_arg "hand_outcomes length must match player hands";
  let player_hand = List.nth player_hands active_hand in
  let current_bet = List.nth hand_bets active_hand in
  {
    player_hand;
    dealer_hand = hand dealer_cards;
    deck = Deck.of_list deck_cards;
    phase;
    outcome;
    player_balance;
    dealer_balance;
    current_bet;
    player_hands;
    active_hand;
    hand_bets;
    hand_done = actual_done;
    hand_outcomes = actual_outcomes;
  }

let count_card target lst =
  List.fold_left (fun acc c -> if c = target then acc + 1 else acc) 0 lst

let string_contains s sub =
  let len_s = String.length s in
  let len_sub = String.length sub in
  let rec loop i =
    if i + len_sub > len_s then false
    else if String.sub s i len_sub = sub then true
    else loop (i + 1)
  in
  len_sub = 0 || loop 0

let assert_substring sub s =
  assert_bool
    (Printf.sprintf "expected string to contain %S, but got %S" sub s)
    (string_contains s sub)

(* Strip ANSI SGR escape sequences (\027[...m) so tests can assert on the plain
   text Render.* produces without depending on color codes. *)
let strip_ansi (s : string) : string =
  let buf = Buffer.create (String.length s) in
  let i = ref 0 in
  let n = String.length s in
  while !i < n do
    if !i + 1 < n && s.[!i] = '\027' && s.[!i + 1] = '[' then begin
      i := !i + 2;
      while !i < n && s.[!i] <> 'm' do
        incr i
      done;
      if !i < n then incr i
    end
    else begin
      Buffer.add_char buf s.[!i];
      incr i
    end
  done;
  Buffer.contents buf

(* Card tests *)

let test_cards _ =
  assert_int_equal 11 (Card.to_int (ace Card.Spades));
  assert_int_equal 2 (Card.to_int (num Card.Hearts 2));
  assert_int_equal 7 (Card.to_int (num Card.Diamonds 7));
  assert_int_equal 10 (Card.to_int (num Card.Clubs 10));
  assert_int_equal 10 (Card.to_int (jack Card.Clubs));
  assert_int_equal 10 (Card.to_int (queen Card.Diamonds));
  assert_int_equal 10 (Card.to_int (king Card.Spades));
  assert_string_equal "Ace of Hearts" (Card.to_string (ace Card.Hearts));
  assert_string_equal "7 of Clubs" (Card.to_string (num Card.Clubs 7));
  assert_string_equal "Queen of Diamonds" (Card.to_string (queen Card.Diamonds));
  assert_string_equal "King of Spades" (Card.to_string (king Card.Spades))

(* Deck tests *)

let test_deck _ =
  assert_int_equal 0 (Deck.size Deck.empty);
  assert_deal_option_equal None (Deck.deal Deck.empty);
  assert_int_equal 416 (Deck.size Deck.standard);
  assert_int_equal 416 (List.length (Deck.to_list Deck.standard));
  assert_int_equal 8 (count_card (ace Card.Spades) (Deck.to_list Deck.standard));
  assert_int_equal 8
    (count_card (num Card.Hearts 10) (Deck.to_list Deck.standard));
  let suit_count suit =
    Deck.to_list Deck.standard
    |> List.filter (fun card -> card.Card.suit = suit)
    |> List.length
  in
  assert_int_equal 104 (suit_count Card.Hearts);
  assert_int_equal 104 (suit_count Card.Diamonds);
  assert_int_equal 104 (suit_count Card.Clubs);
  assert_int_equal 104 (suit_count Card.Spades);
  let c1 = ace Card.Hearts in
  let c2 = num Card.Spades 4 in
  let c3 = king Card.Clubs in
  let d = Deck.of_list [ c1; c2; c3 ] in
  assert_int_equal 3 (Deck.size d);
  (match Deck.deal d with
  | Some (dealt, rest) ->
      assert_card_equal c1 dealt;
      assert_card_list_equal [ c2; c3 ] (Deck.to_list rest);
      assert_int_equal 2 (Deck.size rest)
  | None -> assert_failure "Deck should not be empty");
  let shuffled = Deck.shuffle Deck.standard in
  assert_int_equal 416 (Deck.size shuffled);
  assert_card_list_equal
    (List.sort compare (Deck.to_list Deck.standard))
    (List.sort compare (Deck.to_list shuffled))

(* Hand tests *)

let test_hand_values_and_strings _ =
  assert_int_equal 0 (Hand.value Hand.empty);
  assert_bool_equal false (Hand.is_bust Hand.empty);
  assert_bool_equal false (Hand.is_blackjack Hand.empty);
  assert_string_equal "(empty hand)" (Hand.to_string Hand.empty);
  let c1 = num Card.Spades 5 in
  let c2 = num Card.Hearts 9 in
  let h = hand [ c1; c2 ] in
  assert_card_list_equal [ c2; c1 ] (Hand.cards h);
  assert_int_equal 14 (Hand.value h);
  assert_string_equal "5 of Spades, 9 of Hearts" (Hand.to_string h);
  assert_int_equal 20
    (Hand.value (hand [ king Card.Hearts; queen Card.Spades ]));
  assert_int_equal 18 (Hand.value (hand [ ace Card.Hearts; num Card.Spades 7 ]));
  assert_int_equal 15
    (Hand.value (hand [ ace Card.Hearts; num Card.Spades 9; num Card.Clubs 5 ]));
  assert_int_equal 12 (Hand.value (hand [ ace Card.Hearts; ace Card.Spades ]));
  assert_int_equal 21
    (Hand.value (hand [ ace Card.Hearts; ace Card.Spades; num Card.Clubs 9 ]));
  assert_int_equal 13
    (Hand.value (hand [ ace Card.Hearts; ace Card.Spades; ace Card.Clubs ]));
  assert_int_equal 14
    (Hand.value
       (hand
          [
            ace Card.Hearts; ace Card.Spades; ace Card.Clubs; ace Card.Diamonds;
          ]));
  assert_int_equal 20
    (Hand.value
       (hand [ num Card.Hearts 10; num Card.Spades 9; ace Card.Clubs ]));
  assert_int_equal 16 (Hand.value (hand [ ace Card.Hearts; num Card.Spades 5 ]));
  assert_int_equal 21 (Hand.value (hand [ ace Card.Hearts; king Card.Spades ]));
  assert_int_equal 12
    (Hand.value (hand [ num Card.Hearts 5; num Card.Spades 6; ace Card.Clubs ]));
  assert_int_equal 12
    (Hand.value (hand [ ace Card.Hearts; ace Card.Spades; num Card.Clubs 10 ]));
  assert_int_equal 16
    (Hand.value
       (hand [ ace Card.Hearts; num Card.Spades 5; num Card.Clubs 10 ]));
  assert_bool_equal true
    (Hand.is_bust
       (hand [ king Card.Hearts; queen Card.Spades; num Card.Clubs 5 ]));
  assert_bool_equal false
    (Hand.is_bust (hand [ king Card.Hearts; num Card.Clubs 5 ]));
  assert_bool_equal true
    (Hand.is_blackjack (hand [ ace Card.Hearts; king Card.Spades ]));
  assert_bool_equal true
    (Hand.is_blackjack (hand [ ace Card.Hearts; num Card.Spades 10 ]));
  assert_bool_equal false
    (Hand.is_blackjack
       (hand [ num Card.Hearts 7; num Card.Spades 7; num Card.Clubs 7 ]));
  assert_bool_equal false
    (Hand.is_blackjack (hand [ queen Card.Hearts; king Card.Spades ]))

(* Game setup, betting, and natural blackjack tests *)

let test_game_setup_and_betting _ =
  assert_int_equal 50 Game.initial_balance;
  assert_bool_equal true
    (Game.can_bet ~player_balance:50 ~dealer_balance:50 ~bet:1);
  assert_bool_equal true
    (Game.can_bet ~player_balance:50 ~dealer_balance:50 ~bet:33);
  assert_bool_equal false
    (Game.can_bet ~player_balance:50 ~dealer_balance:50 ~bet:0);
  assert_bool_equal false
    (Game.can_bet ~player_balance:50 ~dealer_balance:50 ~bet:(-1));
  assert_bool_equal false
    (Game.can_bet ~player_balance:4 ~dealer_balance:50 ~bet:5);
  assert_bool_equal false
    (Game.can_bet ~player_balance:50 ~dealer_balance:4 ~bet:5);
  assert_bool_equal false
    (Game.can_bet ~player_balance:50 ~dealer_balance:50 ~bet:34);
  assert_raises (Invalid_argument "Bet must be positive.") (fun () ->
      ignore
        (Game.init_with_deck ~player_balance:50 ~dealer_balance:50 ~bet:0
           Deck.empty));
  assert_raises
    (Invalid_argument "Bet cannot be more than the player's balance.")
    (fun () ->
      ignore
        (Game.init_with_deck ~player_balance:4 ~dealer_balance:50 ~bet:5
           Deck.empty));
  assert_raises
    (Invalid_argument
       "Bet is too large for the dealer to cover a blackjack payout.")
    (fun () ->
      ignore
        (Game.init_with_deck ~player_balance:50 ~dealer_balance:4 ~bet:5
           Deck.empty));
  let random_st =
    Game.init_with_balances ~player_balance:70 ~dealer_balance:80 ~bet:3 ()
  in
  assert_int_equal 3 (Game.current_bet random_st);
  assert_int_equal 412 (Deck.size (Game.deck random_st));
  let random_st2 = Game.init_with_bet 2 in
  assert_int_equal 2 (Game.current_bet random_st2);
  assert_int_equal 412 (Deck.size (Game.deck random_st2));
  ignore (Game.init ())

let test_game_init_with_deck_and_naturals _ =
  let normal_deck =
    Deck.of_list
      [
        num Card.Hearts 9;
        num Card.Clubs 6;
        num Card.Spades 8;
        king Card.Diamonds;
        ace Card.Clubs;
      ]
  in
  let st =
    Game.init_with_deck ~player_balance:50 ~dealer_balance:50 ~bet:5 normal_deck
  in
  assert_phase_equal Player_turn (Game.phase st);
  assert_outcome_option_equal None (Game.outcome st);
  assert_int_equal 17 (Hand.value (Game.player_hand st));
  assert_int_equal 16 (Hand.value (Game.dealer_hand st));
  assert_int_equal 1 (Deck.size (Game.deck st));
  assert_int_list_equal [ 5 ] (Game.hand_bets st);
  assert_int_equal 0 (Game.active_hand st);
  assert_int_equal 1 (List.length (Game.player_hands st));
  let empty_st =
    Game.init_with_deck ~player_balance:50 ~dealer_balance:50 ~bet:5 Deck.empty
  in
  assert_phase_equal Player_turn (Game.phase empty_st);
  assert_int_equal 0 (List.length (Hand.cards (Game.player_hand empty_st)));
  assert_int_equal 0 (List.length (Hand.cards (Game.dealer_hand empty_st)));
  let player_bj =
    Deck.of_list
      [
        ace Card.Hearts; num Card.Clubs 9; king Card.Spades; num Card.Diamonds 7;
      ]
    |> Game.init_with_deck ~player_balance:50 ~dealer_balance:50 ~bet:10
  in
  assert_phase_equal Round_over (Game.phase player_bj);
  assert_outcome_option_equal (Some Player_blackjack) (Game.outcome player_bj);
  assert_int_equal 65 (Game.player_balance player_bj);
  assert_int_equal 35 (Game.dealer_balance player_bj);
  let odd_bj =
    Deck.of_list
      [
        ace Card.Hearts; num Card.Clubs 9; king Card.Spades; num Card.Diamonds 7;
      ]
    |> Game.init_with_deck ~player_balance:50 ~dealer_balance:50 ~bet:5
  in
  assert_int_equal 57 (Game.player_balance odd_bj);
  assert_int_equal 43 (Game.dealer_balance odd_bj);
  let dealer_bj =
    Deck.of_list
      [
        num Card.Hearts 9; ace Card.Clubs; num Card.Spades 8; king Card.Diamonds;
      ]
    |> Game.init_with_deck ~player_balance:50 ~dealer_balance:50 ~bet:5
  in
  assert_outcome_option_equal (Some Dealer_win) (Game.outcome dealer_bj);
  assert_int_equal 45 (Game.player_balance dealer_bj);
  assert_int_equal 55 (Game.dealer_balance dealer_bj);
  let both_bj =
    Deck.of_list
      [ ace Card.Hearts; ace Card.Clubs; king Card.Spades; queen Card.Diamonds ]
    |> Game.init_with_deck ~player_balance:50 ~dealer_balance:50 ~bet:5
  in
  assert_outcome_option_equal (Some Push) (Game.outcome both_bj);
  assert_int_equal 50 (Game.player_balance both_bj);
  assert_int_equal 50 (Game.dealer_balance both_bj)

(* Game hit and stand tests *)

let test_game_hit_and_stand _ =
  let hit_st =
    make_state ~current_bet:5
      [ num Card.Hearts 5; num Card.Spades 6 ]
      [ king Card.Clubs; num Card.Diamonds 7 ]
      [ num Card.Hearts 4 ]
    |> Game.player_hit
  in
  assert_phase_equal Player_turn (Game.phase hit_st);
  assert_outcome_option_equal None (Game.outcome hit_st);
  assert_int_equal 15 (Hand.value (Game.player_hand hit_st));
  assert_int_equal 0 (Deck.size (Game.deck hit_st));
  let bust_st =
    make_state ~current_bet:5
      [ king Card.Hearts; queen Card.Spades ]
      [ num Card.Clubs 10; num Card.Diamonds 7 ]
      [ num Card.Hearts 5 ]
    |> Game.player_hit
  in
  assert_phase_equal Round_over (Game.phase bust_st);
  assert_outcome_option_equal (Some Dealer_win) (Game.outcome bust_st);
  assert_int_equal 45 (Game.player_balance bust_st);
  assert_int_equal 55 (Game.dealer_balance bust_st);
  let empty_hit =
    make_state ~current_bet:5
      [ king Card.Hearts; queen Card.Spades ]
      [ num Card.Clubs 10; num Card.Diamonds 8 ]
      []
    |> Game.player_hit
  in
  assert_outcome_option_equal (Some Player_win) (Game.outcome empty_hit);
  let twenty_one =
    make_state ~current_bet:5
      [ num Card.Hearts 5; num Card.Spades 6 ]
      [ num Card.Clubs 10; num Card.Diamonds 8 ]
      [ king Card.Hearts ]
    |> Game.player_hit |> Game.resolve_after_stand
  in
  assert_outcome_option_equal (Some Player_win) (Game.outcome twenty_one);
  assert_bool_equal false (Game.outcome twenty_one = Some Player_blackjack);
  let round_over_st =
    make_state ~phase:Round_over ~outcome:Dealer_win ~current_bet:5
      [ king Card.Hearts; queen Card.Spades; num Card.Clubs 5 ]
      [ num Card.Clubs 10; num Card.Diamonds 7 ]
      []
  in
  assert_state_equal round_over_st (Game.player_hit round_over_st);
  let dealer_turn_st =
    make_state ~phase:Dealer_turn [ num Card.Hearts 5 ] [] []
  in
  assert_state_equal dealer_turn_st (Game.player_hit dealer_turn_st);
  let stand_win =
    make_state ~current_bet:5
      [ king Card.Hearts; queen Card.Spades ]
      [ num Card.Clubs 10; num Card.Diamonds 8 ]
      []
    |> Game.resolve_after_stand
  in
  assert_outcome_option_equal (Some Player_win) (Game.outcome stand_win);
  let dealer_draws =
    make_state ~current_bet:5
      [ king Card.Hearts; queen Card.Spades ]
      [ num Card.Clubs 10; num Card.Diamonds 6 ]
      [ num Card.Spades 2; king Card.Clubs ]
    |> Game.resolve_after_stand
  in
  assert_int_equal 18 (Hand.value (Game.dealer_hand dealer_draws));
  assert_int_equal 1 (Deck.size (Game.deck dealer_draws));
  assert_outcome_option_equal (Some Player_win) (Game.outcome dealer_draws);
  let dealer_bust =
    make_state ~current_bet:5
      [ king Card.Hearts; num Card.Spades 7 ]
      [ num Card.Clubs 10; num Card.Diamonds 6 ]
      [ king Card.Clubs ]
    |> Game.resolve_after_stand
  in
  assert_outcome_option_equal (Some Player_win) (Game.outcome dealer_bust);
  let dealer_wins =
    make_state ~current_bet:5
      [ king Card.Hearts; num Card.Spades 7 ]
      [ num Card.Clubs 10; num Card.Diamonds 9 ]
      []
    |> Game.resolve_after_stand
  in
  assert_outcome_option_equal (Some Dealer_win) (Game.outcome dealer_wins);
  let push =
    make_state ~current_bet:5
      [ king Card.Hearts; num Card.Spades 8 ]
      [ num Card.Clubs 10; num Card.Diamonds 8 ]
      []
    |> Game.resolve_after_stand
  in
  assert_outcome_option_equal (Some Push) (Game.outcome push);
  assert_int_equal 50 (Game.player_balance push);
  assert_int_equal 50 (Game.dealer_balance push);
  let dealer_turn_resolved =
    make_state ~phase:Dealer_turn ~current_bet:5
      [ king Card.Hearts; num Card.Spades 8 ]
      [ num Card.Clubs 10; num Card.Diamonds 9 ]
      []
    |> Game.resolve_after_stand
  in
  assert_phase_equal Round_over (Game.phase dealer_turn_resolved);
  let already_over =
    make_state ~phase:Round_over ~outcome:Player_win ~current_bet:5
      [ king Card.Hearts; queen Card.Spades ]
      [ num Card.Clubs 10; num Card.Diamonds 8 ]
      []
  in
  assert_state_equal already_over (Game.resolve_after_stand already_over)

let test_game_accessors _ =
  let st =
    make_state ~player_balance:37 ~dealer_balance:63 ~current_bet:12
      [ num Card.Hearts 9; num Card.Spades 8 ]
      [ num Card.Clubs 10; num Card.Diamonds 6 ]
      [ ace Card.Hearts ]
  in
  assert_int_equal 17 (Hand.value (Game.player_hand st));
  assert_int_equal 16 (Hand.value (Game.dealer_hand st));
  assert_int_equal 1 (Deck.size (Game.deck st));
  assert_phase_equal Player_turn (Game.phase st);
  assert_outcome_option_equal None (Game.outcome st);
  assert_int_equal 37 (Game.player_balance st);
  assert_int_equal 63 (Game.dealer_balance st);
  assert_int_equal 12 (Game.current_bet st);
  assert_hand_list_equal [ st.player_hand ] (Game.player_hands st);
  assert_int_equal 0 (Game.active_hand st);
  assert_int_list_equal [ 12 ] (Game.hand_bets st);
  assert_outcome_option_list_equal [ None ] (Game.hand_outcomes st)

(* Split tests *)

let test_game_split _ =
  let pair =
    make_state ~current_bet:5
      [ num Card.Hearts 8; num Card.Spades 8 ]
      [ king Card.Clubs; num Card.Diamonds 6 ]
      [ num Card.Hearts 3; num Card.Clubs 4; king Card.Spades ]
  in
  assert_bool_equal true (Game.can_split pair);
  assert_bool_equal false
    (Game.can_split
       (make_state ~current_bet:5
          [ num Card.Hearts 8; num Card.Spades 9 ]
          [ king Card.Clubs; num Card.Diamonds 6 ]
          []));
  assert_bool_equal false
    (Game.can_split
       (make_state ~current_bet:5
          [ num Card.Hearts 8; num Card.Spades 8; num Card.Clubs 2 ]
          [ king Card.Clubs; num Card.Diamonds 6 ]
          []));
  assert_bool_equal false (Game.can_split { pair with phase = Dealer_turn });
  assert_bool_equal false
    (Game.can_split { pair with phase = Round_over; outcome = Some Push });
  assert_bool_equal false
    (Game.can_split
       (make_state ~player_balance:50 ~dealer_balance:50 ~current_bet:30
          [ num Card.Hearts 8; num Card.Spades 8 ]
          [ king Card.Clubs; num Card.Diamonds 6 ]
          []));
  let split_st = Game.player_split pair in
  assert_int_equal 2 (List.length (Game.player_hands split_st));
  assert_int_list_equal [ 5; 5 ] (Game.hand_bets split_st);
  assert_int_equal 0 (Game.active_hand split_st);
  assert_int_equal 11 (Hand.value (List.nth (Game.player_hands split_st) 0));
  assert_int_equal 12 (Hand.value (List.nth (Game.player_hands split_st) 1));
  assert_int_equal 1 (Deck.size (Game.deck split_st));
  let no_split =
    make_state ~current_bet:5
      [ num Card.Hearts 8; num Card.Spades 9 ]
      [ king Card.Clubs; num Card.Diamonds 6 ]
      []
  in
  assert_state_equal no_split (Game.player_split no_split);
  let empty_split =
    make_state ~current_bet:5
      [ num Card.Hearts 8; num Card.Spades 8 ]
      [ king Card.Clubs; num Card.Diamonds 6 ]
      []
    |> Game.player_split
  in
  assert_int_equal 2 (List.length (Game.player_hands empty_split));
  assert_int_equal 8 (Hand.value (List.nth (Game.player_hands empty_split) 0));
  assert_int_equal 8 (Hand.value (List.nth (Game.player_hands empty_split) 1));
  let active_second =
    make_multi_state ~active_hand:1
      [
        [ num Card.Hearts 10; num Card.Clubs 2 ];
        [ num Card.Hearts 9; num Card.Spades 9 ];
      ]
      [ king Card.Clubs; num Card.Diamonds 6 ]
      [ 5; 5 ]
      [ num Card.Hearts 2; num Card.Clubs 3; king Card.Spades ]
    |> Game.player_split
  in
  assert_int_equal 3 (List.length (Game.player_hands active_second));
  assert_int_list_equal [ 5; 5; 5 ] (Game.hand_bets active_second);
  assert_int_equal 1 (Game.active_hand active_second)

let test_game_split_resolution _ =
  let split_once =
    make_state ~current_bet:5
      [ num Card.Hearts 8; num Card.Spades 8 ]
      [ king Card.Clubs; num Card.Diamonds 6 ]
      [ num Card.Hearts 3; num Card.Clubs 4; king Card.Spades ]
    |> Game.player_split
  in
  let after_first_stand = Game.resolve_after_stand split_once in
  assert_phase_equal Player_turn (Game.phase after_first_stand);
  assert_int_equal 1 (Game.active_hand after_first_stand);
  assert_outcome_option_equal None (Game.outcome after_first_stand);
  let both_win = Game.resolve_after_stand after_first_stand in
  assert_phase_equal Round_over (Game.phase both_win);
  assert_outcome_option_equal (Some Player_win) (Game.outcome both_win);
  assert_outcome_option_list_equal
    [ Some Player_win; Some Player_win ]
    (Game.hand_outcomes both_win);
  assert_int_equal 60 (Game.player_balance both_win);
  assert_int_equal 40 (Game.dealer_balance both_win);
  let all_lose =
    make_state ~current_bet:5
      [ num Card.Hearts 8; num Card.Spades 8 ]
      [ king Card.Clubs; queen Card.Diamonds ]
      [ num Card.Hearts 3; num Card.Clubs 4 ]
    |> Game.player_split |> Game.resolve_after_stand |> Game.resolve_after_stand
  in
  assert_outcome_option_equal (Some Dealer_win) (Game.outcome all_lose);
  assert_outcome_option_list_equal
    [ Some Dealer_win; Some Dealer_win ]
    (Game.hand_outcomes all_lose);
  assert_int_equal 40 (Game.player_balance all_lose);
  let mixed_push =
    make_state ~current_bet:5
      [ num Card.Hearts 10; num Card.Spades 10 ]
      [ king Card.Clubs; queen Card.Diamonds ]
      [ num Card.Hearts 8; king Card.Spades ]
    |> Game.player_split |> Game.resolve_after_stand |> Game.resolve_after_stand
  in
  assert_outcome_option_equal (Some Push) (Game.outcome mixed_push);
  assert_outcome_option_list_equal
    [ Some Dealer_win; Some Push ]
    (Game.hand_outcomes mixed_push);
  assert_int_equal 45 (Game.player_balance mixed_push);
  let bust_then_advance =
    make_state ~current_bet:5
      [ num Card.Hearts 10; num Card.Spades 10 ]
      [ king Card.Clubs; num Card.Diamonds 6 ]
      [ num Card.Hearts 8; num Card.Clubs 4; king Card.Spades ]
    |> Game.player_split |> Game.player_hit
  in
  assert_phase_equal Player_turn (Game.phase bust_then_advance);
  assert_int_equal 1 (Game.active_hand bust_then_advance);
  assert_outcome_option_list_equal [ Some Dealer_win; None ]
    (Game.hand_outcomes bust_then_advance);
  let partial_blackjack =
    make_multi_state ~active_hand:1 ~hand_done:[ true; false ]
      ~hand_outcomes:[ Some Player_blackjack; None ]
      [
        [ ace Card.Hearts; king Card.Spades ];
        [ num Card.Hearts 10; num Card.Spades 5 ];
      ]
      [ king Card.Clubs; queen Card.Diamonds ]
      [ 10; 5 ] []
    |> Game.resolve_after_stand
  in
  assert_outcome_option_equal (Some Player_win) (Game.outcome partial_blackjack);
  assert_outcome_option_list_equal
    [ Some Player_blackjack; Some Dealer_win ]
    (Game.hand_outcomes partial_blackjack);
  assert_int_equal 60 (Game.player_balance partial_blackjack);
  assert_int_equal 40 (Game.dealer_balance partial_blackjack)

(* Double down tests *)

let test_game_double_down _ =
  let can_double =
    make_state ~current_bet:5
      [ num Card.Hearts 5; num Card.Spades 6 ]
      [ king Card.Clubs; num Card.Diamonds 6 ]
      [ num Card.Hearts 9 ]
  in
  assert_bool_equal true (Game.can_double_down can_double);
  assert_bool_equal false
    (Game.can_double_down
       (make_state ~current_bet:5
          [ num Card.Hearts 5; num Card.Spades 3; num Card.Clubs 3 ]
          [ king Card.Clubs; num Card.Diamonds 6 ]
          [ num Card.Hearts 9 ]));
  assert_bool_equal false
    (Game.can_double_down { can_double with phase = Dealer_turn });
  assert_bool_equal false
    (Game.can_double_down
       { can_double with phase = Round_over; outcome = Some Push });
  assert_bool_equal false
    (Game.can_double_down
       (make_state ~player_balance:9 ~dealer_balance:50 ~current_bet:5
          [ num Card.Hearts 5; num Card.Spades 6 ]
          [ king Card.Clubs; num Card.Diamonds 6 ]
          [ num Card.Hearts 9 ]));
  let win =
    make_state ~current_bet:5
      [ num Card.Hearts 5; num Card.Spades 6 ]
      [ king Card.Clubs; num Card.Diamonds 6 ]
      [ num Card.Hearts 9; king Card.Spades ]
    |> Game.player_double_down
  in
  assert_phase_equal Round_over (Game.phase win);
  assert_outcome_option_equal (Some Player_win) (Game.outcome win);
  assert_int_list_equal [ 10 ] (Game.hand_bets win);
  assert_int_equal 60 (Game.player_balance win);
  let bust =
    make_state ~current_bet:5
      [ num Card.Hearts 10; num Card.Spades 9 ]
      [ king Card.Clubs; num Card.Diamonds 6 ]
      [ king Card.Spades ]
    |> Game.player_double_down
  in
  assert_outcome_option_equal (Some Dealer_win) (Game.outcome bust);
  assert_int_list_equal [ 10 ] (Game.hand_bets bust);
  assert_int_equal 40 (Game.player_balance bust);
  let empty_deck =
    make_state ~current_bet:5
      [ num Card.Hearts 5; num Card.Spades 6 ]
      [ king Card.Clubs; num Card.Diamonds 7 ]
      []
    |> Game.player_double_down
  in
  assert_int_list_equal [ 5 ] (Game.hand_bets empty_deck);
  assert_outcome_option_equal (Some Dealer_win) (Game.outcome empty_deck);
  assert_int_equal 45 (Game.player_balance empty_deck);
  let cannot_double =
    make_state ~player_balance:9 ~current_bet:5
      [ num Card.Hearts 5; num Card.Spades 6 ]
      [ king Card.Clubs; num Card.Diamonds 6 ]
      [ num Card.Hearts 9 ]
  in
  assert_state_equal cannot_double (Game.player_double_down cannot_double);
  let active_second =
    make_multi_state ~active_hand:1 ~hand_done:[ true; false ]
      [
        [ num Card.Hearts 10; num Card.Clubs 2 ];
        [ num Card.Hearts 5; num Card.Spades 6 ];
      ]
      [ king Card.Clubs; num Card.Diamonds 6 ]
      [ 5; 5 ]
      [ num Card.Hearts 9; king Card.Spades ]
    |> Game.player_double_down
  in
  assert_int_list_equal [ 5; 10 ] (Game.hand_bets active_second);
  assert_outcome_option_list_equal
    [ Some Player_win; Some Player_win ]
    (Game.hand_outcomes active_second);
  assert_int_equal 65 (Game.player_balance active_second)

(* Analysis tests *)

let test_analysis_probabilities_and_distribution _ =
  let all_bust =
    make_state
      [ king Card.Hearts; queen Card.Diamonds ]
      []
      [ num Card.Spades 5; queen Card.Clubs ]
  in
  assert_float_equal 1.0 (Analysis.hit_bust_probability all_bust);
  let none_bust =
    make_state
      [ num Card.Hearts 2; num Card.Diamonds 3 ]
      []
      [ num Card.Spades 5; queen Card.Clubs ]
  in
  assert_float_equal 0.0 (Analysis.hit_bust_probability none_bust);
  let mixed =
    make_state
      [ num Card.Hearts 10; num Card.Diamonds 6 ]
      []
      [
        num Card.Spades 5;
        num Card.Spades 6;
        queen Card.Clubs;
        num Card.Clubs 3;
        num Card.Diamonds 2;
      ]
  in
  assert_float_equal 0.4 (Analysis.hit_bust_probability mixed);
  let dist =
    make_state
      [ num Card.Hearts 10; num Card.Spades 5 ]
      []
      [ num Card.Clubs 2; num Card.Diamonds 4; num Card.Hearts 2 ]
  in
  assert_distribution_equal
    [ (17, 2. /. 3.); (19, 1. /. 3.) ]
    (Analysis.next_scores_distribution dist);
  let ace_dist =
    make_state
      [ ace Card.Hearts; num Card.Spades 5 ]
      []
      [ ace Card.Clubs; num Card.Diamonds 4 ]
  in
  assert_distribution_equal
    [ (17, 1. /. 2.); (20, 1. /. 2.) ]
    (Analysis.next_scores_distribution ace_dist)

let test_analysis_recommendation _ =
  let expect expected ?(player_balance = 50) ?(current_bet = 5) player_cards
      dealer_cards =
    let st =
      make_state ~player_balance ~current_bet player_cards dealer_cards
        [ num Card.Hearts 2 ]
    in
    assert_string_equal expected (Analysis.recommendation st)
  in
  expect "The optimal move is to stick."
    [ num Card.Hearts 7; queen Card.Diamonds ]
    [ num Card.Spades 6 ];
  expect "The optimal move is to stick."
    [ king Card.Hearts; num Card.Spades 6 ]
    [ king Card.Clubs; num Card.Diamonds 6 ];
  expect "The optimal move is to hit."
    [ king Card.Hearts; num Card.Spades 6 ]
    [ king Card.Clubs; num Card.Diamonds 7 ];
  expect "The optimal move is to stick."
    [ king Card.Hearts; num Card.Spades 2 ]
    [ king Card.Clubs; num Card.Diamonds 4 ];
  expect "The optimal move is to hit."
    [ king Card.Hearts; num Card.Spades 2 ]
    [ king Card.Clubs; num Card.Diamonds 3 ];
  expect "The optimal move is to double down."
    [ num Card.Hearts 6; num Card.Diamonds 5 ]
    [ num Card.Spades 7 ];
  expect "The optimal move is to hit." ~player_balance:5
    [ num Card.Hearts 5; num Card.Spades 6 ]
    [ king Card.Clubs; num Card.Diamonds 6 ];
  expect "The optimal move is to double down."
    [ num Card.Hearts 4; num Card.Spades 6 ]
    [ king Card.Clubs; num Card.Diamonds 9 ];
  expect "The optimal move is to hit."
    [ num Card.Hearts 4; num Card.Spades 6 ]
    [ num Card.Clubs 3; king Card.Diamonds ];
  expect "The optimal move is to hit." ~player_balance:5
    [ num Card.Hearts 4; num Card.Spades 6 ]
    [ king Card.Clubs; num Card.Diamonds 6 ];
  expect "The optimal move is to double down."
    [ num Card.Hearts 4; num Card.Spades 5 ]
    [ king Card.Clubs; num Card.Diamonds 3 ];
  expect "The optimal move is to hit."
    [ num Card.Hearts 4; num Card.Spades 5 ]
    [ king Card.Clubs; num Card.Diamonds 2 ];
  expect "The optimal move is to hit." ~player_balance:5
    [ num Card.Hearts 4; num Card.Spades 5 ]
    [ king Card.Clubs; num Card.Diamonds 6 ];
  expect "The optimal move is to hit."
    [ num Card.Hearts 3; num Card.Spades 5 ]
    [ king Card.Clubs; num Card.Diamonds 6 ];
  let no_dealer = make_state [ num Card.Hearts 10; num Card.Spades 7 ] [] [] in
  assert_raises (Failure "Error: dealer has no cards") (fun () ->
      ignore (Analysis.recommendation no_dealer))

let test_analysis_split_recommendation _ =
  let expect expected ?(current_bet = 5) player_cards dealer_cards =
    let st =
      make_state ~current_bet player_cards dealer_cards [ num Card.Hearts 2 ]
    in
    assert_string_equal expected (Analysis.split_recommendation st)
  in
  expect "The optimal move is to split."
    [ num Card.Hearts 4; num Card.Diamonds 4 ]
    [ num Card.Spades 5 ];
  expect "The optimal move is to not split."
    [ num Card.Hearts 5; num Card.Diamonds 5 ]
    [ num Card.Spades 6 ];
  expect "Splitting is not available."
    [ num Card.Hearts 5; queen Card.Diamonds ]
    [ num Card.Spades 6 ];
  expect "The optimal move is to split."
    [ num Card.Hearts 3; num Card.Spades 3 ]
    [ king Card.Clubs; num Card.Diamonds 6 ];
  expect "The optimal move is to not split."
    [ num Card.Hearts 3; num Card.Spades 3 ]
    [ king Card.Clubs; num Card.Diamonds 8 ];
  expect "The optimal move is to split."
    [ num Card.Hearts 2; num Card.Spades 2 ]
    [ king Card.Clubs; num Card.Diamonds 5 ];
  expect "The optimal move is to not split."
    [ num Card.Hearts 2; num Card.Spades 2 ]
    [ num Card.Diamonds 4; king Card.Clubs ];
  expect "Splitting is not available."
    [ num Card.Hearts 7; num Card.Spades 6 ]
    [ king Card.Clubs; num Card.Diamonds 6 ];
  let no_dealer =
    make_state ~current_bet:5
      [ num Card.Hearts 4; num Card.Spades 4 ]
      []
      [ num Card.Hearts 2 ]
  in
  assert_raises (Failure "Dealer has no cards") (fun () ->
      ignore (Analysis.split_recommendation no_dealer))

(* Render tests *)

let test_render _ =
  assert_string_equal "(empty)" (Render.dealer_hidden Hand.empty);
  let dealer = hand [ num Card.Clubs 10; num Card.Hearts 7 ] in
  assert_string_equal "10 of Clubs, [hidden]" (Render.dealer_hidden dealer);
  let player = hand [ ace Card.Hearts; king Card.Spades ] in
  assert_string_equal "Ace of Hearts, King of Spades (value: 21)"
    (strip_ansi (Render.hand_full player));
  let st =
    make_state ~player_balance:65 ~dealer_balance:35 ~current_bet:10
      [ ace Card.Hearts; king Card.Spades ]
      [ num Card.Clubs 10; num Card.Hearts 7 ]
      []
  in
  let hidden = strip_ansi (Render.state st) in
  assert_substring "Dealer chips: 35" hidden;
  assert_substring "Your chips:   65" hidden;
  assert_substring "Current bet:  10" hidden;
  assert_substring "[hidden]" hidden;
  let shown = strip_ansi (Render.state ~hide_dealer:false st) in
  assert_substring "10 of Clubs, 7 of Hearts (value: 17)" shown;
  assert_string_equal "You win!" (strip_ansi (Render.outcome Player_win));
  assert_string_equal "Blackjack! You win 3:2."
    (strip_ansi (Render.outcome Player_blackjack));
  assert_string_equal "Dealer wins." (strip_ansi (Render.outcome Dealer_win));
  assert_string_equal "Push (tie)." (strip_ansi (Render.outcome Push))

let suite =
  "blackjack tests"
  >::: [
         "cards" >:: test_cards;
         "deck" >:: test_deck;
         "hand values and strings" >:: test_hand_values_and_strings;
         "game setup and betting" >:: test_game_setup_and_betting;
         "game init with deck and naturals"
         >:: test_game_init_with_deck_and_naturals;
         "game hit and stand" >:: test_game_hit_and_stand;
         "game accessors" >:: test_game_accessors;
         "game split" >:: test_game_split;
         "game split resolution" >:: test_game_split_resolution;
         "game double down" >:: test_game_double_down;
         "analysis probabilities and distribution"
         >:: test_analysis_probabilities_and_distribution;
         "analysis recommendation" >:: test_analysis_recommendation;
         "analysis split recommendation" >:: test_analysis_split_recommendation;
         "render" >:: test_render;
       ]

let () = run_test_tt_main suite
