let dealer_hidden (h : Hand.t) : string =
  match List.rev (Hand.cards h) with
  | [] -> "(empty)"
  | first :: _ -> Card.to_string first ^ ", [hidden]"

let hand_full (h : Hand.t) : string =
  let value_str = ANSITerminal.sprintf [ Bold ] "value: %d" (Hand.value h) in
  Printf.sprintf "%s (%s)" (Hand.to_string h) value_str

let chips_str n = ANSITerminal.sprintf [ Foreground Yellow ] "%d" n

let outcome = function
  | Game.Player_win -> ANSITerminal.sprintf [ Foreground Green ] "You win!"
  | Game.Player_blackjack ->
      ANSITerminal.sprintf [ Foreground Green; Bold ] "Blackjack! You win 3:2."
  | Game.Dealer_win -> ANSITerminal.sprintf [ Foreground Red ] "Dealer wins."
  | Game.Push -> ANSITerminal.sprintf [ Foreground Yellow ] "Push (tie)."

let hand_lines (st : Game.state) : string =
  let hands = Game.player_hands st in
  let bets = Game.hand_bets st in
  let outcomes = Game.hand_outcomes st in
  let active = Game.active_hand st in

  List.mapi
    (fun i hand ->
      let marker =
        if i = active && Game.phase st = Game.Player_turn then "*" else " "
      in
      let bet = List.nth bets i in
      let result =
        match List.nth outcomes i with
        | None -> ""
        | Some o -> " -- " ^ outcome o
      in
      Printf.sprintf "%s Hand %d, bet %d: %s%s" marker (i + 1) bet
        (hand_full hand) result)
    hands
  |> String.concat "\n"

let state ?(hide_dealer = true) (st : Game.state) : string =
  let dealer_str =
    if hide_dealer then dealer_hidden (Game.dealer_hand st)
    else hand_full (Game.dealer_hand st)
  in
  if List.length (Game.player_hands st) > 1 then
    Printf.sprintf
      "\n\
       Dealer chips: %s\n\
       Your chips:   %s\n\
       Current bet:  %d\n\
       Dealer: %s\n\
       Your hands:\n\
       %s"
      (chips_str (Game.dealer_balance st))
      (chips_str (Game.player_balance st))
      (Game.current_bet st) dealer_str (hand_lines st)
  else
    Printf.sprintf
      "\n\
       Dealer chips: %s\n\
       Your chips:   %s\n\
       Current bet:  %d\n\
       Dealer: %s\n\
       You:    %s"
      (chips_str (Game.dealer_balance st))
      (chips_str (Game.player_balance st))
      (Game.current_bet st) dealer_str
      (hand_full (Game.player_hand st))
