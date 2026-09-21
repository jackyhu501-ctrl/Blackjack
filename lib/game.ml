type phase =
  | Player_turn
  | Dealer_turn
  | Round_over

type outcome =
  | Player_win
  | Player_blackjack
  | Dealer_win
  | Push

type state = {
  player_hand : Hand.t;
  dealer_hand : Hand.t;
  deck : Deck.t;
  phase : phase;
  outcome : outcome option;
  player_balance : int;
  dealer_balance : int;
  current_bet : int;
  player_hands : Hand.t list;
  active_hand : int;
  hand_bets : int list;
  hand_done : bool list;
  hand_outcomes : outcome option list;
}

let initial_balance = 50
let player_hand st = st.player_hand
let dealer_hand st = st.dealer_hand
let deck st = st.deck
let phase st = st.phase
let outcome st = st.outcome
let player_balance st = st.player_balance
let dealer_balance st = st.dealer_balance
let current_bet st = st.current_bet
let player_hands st = st.player_hands
let active_hand st = st.active_hand
let hand_bets st = st.hand_bets
let hand_outcomes st = st.hand_outcomes

(* 3:2 blackjack payout. Because chip balances are ints, odd bets round down.
   For example, a 10-chip blackjack pays 15 chips, while a 5-chip blackjack pays
   7 chips. *)
let blackjack_profit bet = bet * 3 / 2

let rec nth_exn lst n =
  match (lst, n) with
  | [], _ -> invalid_arg "index out of bounds"
  | x :: _, 0 -> x
  | _ :: xs, n -> nth_exn xs (n - 1)

let rec update_nth lst n value =
  match (lst, n) with
  | [], _ -> invalid_arg "index out of bounds"
  | _ :: xs, 0 -> value :: xs
  | x :: xs, n -> x :: update_nth xs (n - 1) value

let rec insert_after lst n value =
  match (lst, n) with
  | [], _ -> invalid_arg "index out of bounds"
  | x :: xs, 0 -> x :: value :: xs
  | x :: xs, n -> x :: insert_after xs (n - 1) value

let rec find_index_from pred i lst =
  match lst with
  | [] -> None
  | x :: xs -> if pred x then Some i else find_index_from pred (i + 1) xs

let sum_ints = List.fold_left ( + ) 0
let current_hand st = nth_exn st.player_hands st.active_hand
let current_hand_bet st = nth_exn st.hand_bets st.active_hand

let sync_current_hand st =
  { st with player_hand = current_hand st; current_bet = current_hand_bet st }

let total_bet_exposure st = sum_ints st.hand_bets

let can_add_bet st extra_bet =
  extra_bet > 0
  && total_bet_exposure st + extra_bet <= st.player_balance
  && total_bet_exposure st + extra_bet <= st.dealer_balance

let can_bet ~player_balance ~dealer_balance ~bet =
  bet > 0 && bet <= player_balance && blackjack_profit bet <= dealer_balance

let validate_bet ~player_balance ~dealer_balance ~bet =
  if bet <= 0 then invalid_arg "Bet must be positive."
  else if bet > player_balance then
    invalid_arg "Bet cannot be more than the player's balance."
  else if blackjack_profit bet > dealer_balance then
    invalid_arg "Bet is too large for the dealer to cover a blackjack payout."
  else ()

(* Bust loses; otherwise higher value wins, tie is a push. A natural blackjack
   is handled separately during the initial deal so that it can pay 3:2. *)
let determine_outcome (player : Hand.t) (dealer : Hand.t) : outcome =
  if Hand.is_bust player then Dealer_win
  else if Hand.is_bust dealer then Player_win
  else
    let player_value = Hand.value player in
    let dealer_value = Hand.value dealer in
    if player_value > dealer_value then Player_win
    else if dealer_value > player_value then Dealer_win
    else Push

let settle_balances player_balance dealer_balance bet outcome =
  match outcome with
  | Player_win -> (player_balance + bet, dealer_balance - bet)
  | Player_blackjack ->
      let profit = blackjack_profit bet in
      (player_balance + profit, dealer_balance - profit)
  | Dealer_win -> (player_balance - bet, dealer_balance + bet)
  | Push -> (player_balance, dealer_balance)

let finish_round (st : state) (final_outcome : outcome) : state =
  let new_player_balance, new_dealer_balance =
    settle_balances st.player_balance st.dealer_balance st.current_bet
      final_outcome
  in
  {
    st with
    phase = Round_over;
    outcome = Some final_outcome;
    player_balance = new_player_balance;
    dealer_balance = new_dealer_balance;
  }

let aggregate_outcome outcomes =
  match outcomes with
  | [ o ] -> o
  | _ ->
      if
        List.exists (( = ) Player_win) outcomes
        || List.exists (( = ) Player_blackjack) outcomes
      then Player_win
      else if List.for_all (( = ) Dealer_win) outcomes then Dealer_win
      else Push

let finalize_round st final_dealer_hand final_deck final_outcomes =
  let new_player_balance, new_dealer_balance =
    List.fold_left2
      (fun (pb, db) bet outcome -> settle_balances pb db bet outcome)
      (st.player_balance, st.dealer_balance)
      st.hand_bets final_outcomes
  in
  {
    st with
    dealer_hand = final_dealer_hand;
    deck = final_deck;
    phase = Round_over;
    outcome = Some (aggregate_outcome final_outcomes);
    player_balance = new_player_balance;
    dealer_balance = new_dealer_balance;
    hand_outcomes = List.map (fun o -> Some o) final_outcomes;
  }
  |> sync_current_hand

(* Deal the top card of [d] into [h]. No-op when [d] is empty. *)
let deal_one (h : Hand.t) (d : Deck.t) : Hand.t * Deck.t =
  match Deck.deal d with
  | None -> (h, d)
  | Some (card, rest_deck) -> (Hand.add_card card h, rest_deck)

let init_with_deck ~player_balance ~dealer_balance ~bet deck =
  validate_bet ~player_balance ~dealer_balance ~bet;

  let player_hand_1, deck_1 = deal_one Hand.empty deck in
  let dealer_hand_1, deck_2 = deal_one Hand.empty deck_1 in
  let player_hand_2, deck_3 = deal_one player_hand_1 deck_2 in
  let dealer_hand_2, deck_4 = deal_one dealer_hand_1 deck_3 in

  let st =
    {
      player_hand = player_hand_2;
      dealer_hand = dealer_hand_2;
      deck = deck_4;
      phase = Player_turn;
      outcome = None;
      player_balance;
      dealer_balance;
      current_bet = bet;
      player_hands = [ player_hand_2 ];
      active_hand = 0;
      hand_bets = [ bet ];
      hand_done = [ false ];
      hand_outcomes = [ None ];
    }
  in

  if Hand.is_blackjack player_hand_2 && Hand.is_blackjack dealer_hand_2 then
    finish_round st Push
  else if Hand.is_blackjack player_hand_2 then finish_round st Player_blackjack
  else if Hand.is_blackjack dealer_hand_2 then finish_round st Dealer_win
  else st

let init_with_balances ~player_balance ~dealer_balance ~bet () : state =
  Random.self_init ();
  init_with_deck ~player_balance ~dealer_balance ~bet
    (Deck.shuffle Deck.standard)

let init_with_bet bet : state =
  init_with_balances ~player_balance:initial_balance
    ~dealer_balance:initial_balance ~bet ()

let init () : state = init_with_bet 1

let rec dealer_play (dealer_h : Hand.t) (d : Deck.t) : Hand.t * Deck.t =
  if Hand.value dealer_h < 17 then
    match Deck.deal d with
    | None -> (dealer_h, d)
    | Some (card, rest_deck) ->
        dealer_play (Hand.add_card card dealer_h) rest_deck
  else (dealer_h, d)

let complete_if_needed st =
  match find_index_from (fun done_ -> not done_) 0 st.hand_done with
  | Some next_i ->
      { st with active_hand = next_i; phase = Player_turn } |> sync_current_hand
  | None ->
      let final_dealer_hand, final_deck = dealer_play st.dealer_hand st.deck in
      let final_outcomes =
        List.map2
          (fun hand partial_outcome ->
            match partial_outcome with
            | Some outcome -> outcome
            | None -> determine_outcome hand final_dealer_hand)
          st.player_hands st.hand_outcomes
      in
      finalize_round st final_dealer_hand final_deck final_outcomes

let player_hit (st : state) : state =
  match st.phase with
  | Round_over -> st
  | Dealer_turn -> st
  | Player_turn -> (
      match Deck.deal st.deck with
      | None ->
          complete_if_needed
            {
              st with
              hand_done = update_nth st.hand_done st.active_hand true;
              hand_outcomes =
                update_nth st.hand_outcomes st.active_hand
                  (Some (determine_outcome (current_hand st) st.dealer_hand));
            }
      | Some (card, rest_deck) ->
          let old_hand = current_hand st in
          let new_player_hand = Hand.add_card card old_hand in
          let new_hands =
            update_nth st.player_hands st.active_hand new_player_hand
          in
          let st_after_hit =
            {
              st with
              player_hand = new_player_hand;
              player_hands = new_hands;
              deck = rest_deck;
            }
          in
          if Hand.is_bust new_player_hand then
            complete_if_needed
              {
                st_after_hit with
                hand_done = update_nth st.hand_done st.active_hand true;
                hand_outcomes =
                  update_nth st.hand_outcomes st.active_hand (Some Dealer_win);
              }
          else st_after_hit)

let resolve_after_stand (st : state) : state =
  match st.phase with
  | Round_over -> st
  | Dealer_turn | Player_turn ->
      complete_if_needed
        { st with hand_done = update_nth st.hand_done st.active_hand true }

let same_rank c1 c2 = c1.Card.rank = c2.Card.rank

let can_split st =
  match st.phase with
  | Player_turn -> (
      match Hand.cards (current_hand st) with
      | [ c1; c2 ] -> same_rank c1 c2 && can_add_bet st (current_hand_bet st)
      | _ -> false)
  | Dealer_turn | Round_over -> false

let player_split st =
  if not (can_split st) then st
  else
    match Hand.cards (current_hand st) with
    | [ c1; c2 ] ->
        let bet = current_hand_bet st in

        let hand1 = Hand.add_card c1 Hand.empty in
        let hand2 = Hand.add_card c2 Hand.empty in

        let hand1, deck1 = deal_one hand1 st.deck in
        let hand2, deck2 = deal_one hand2 deck1 in

        let updated_hands = update_nth st.player_hands st.active_hand hand1 in
        let new_player_hands =
          insert_after updated_hands st.active_hand hand2
        in

        let new_hand_bets = insert_after st.hand_bets st.active_hand bet in

        let updated_done = update_nth st.hand_done st.active_hand false in
        let new_hand_done = insert_after updated_done st.active_hand false in

        let updated_outcomes =
          update_nth st.hand_outcomes st.active_hand None
        in
        let new_hand_outcomes =
          insert_after updated_outcomes st.active_hand None
        in

        {
          st with
          player_hand = hand1;
          deck = deck2;
          player_hands = new_player_hands;
          hand_bets = new_hand_bets;
          hand_done = new_hand_done;
          hand_outcomes = new_hand_outcomes;
        }
        |> sync_current_hand
    | _ -> st

let can_double_down st =
  match st.phase with
  | Player_turn ->
      List.length (Hand.cards (current_hand st)) = 2
      && can_add_bet st (current_hand_bet st)
  | Dealer_turn | Round_over -> false

let player_double_down st =
  if not (can_double_down st) then st
  else
    match Deck.deal st.deck with
    | None -> resolve_after_stand st
    | Some (card, rest_deck) ->
        let new_hand = Hand.add_card card (current_hand st) in
        let new_bet = current_hand_bet st * 2 in
        let maybe_outcome =
          if Hand.is_bust new_hand then Some Dealer_win else None
        in
        complete_if_needed
          {
            st with
            player_hand = new_hand;
            deck = rest_deck;
            player_hands = update_nth st.player_hands st.active_hand new_hand;
            hand_bets = update_nth st.hand_bets st.active_hand new_bet;
            hand_done = update_nth st.hand_done st.active_hand true;
            hand_outcomes =
              update_nth st.hand_outcomes st.active_hand maybe_outcome;
          }
