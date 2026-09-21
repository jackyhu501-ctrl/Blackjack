open Blackjack

let hints_on = ref true

let print_state ?(hide_dealer = true) (st : Game.state) : unit =
  print_endline (Render.state ~hide_dealer st)

let print_analysis (st : Game.state) : unit =
  if (not !hints_on) || Deck.to_list (Game.deck st) = [] then ()
  else begin
    let bust_p = Analysis.hit_bust_probability st in
    let dist = Analysis.next_scores_distribution st in
    let buf = Buffer.create 256 in
    Buffer.add_string buf "Analysis:\n";
    Buffer.add_string buf
      (Printf.sprintf "  Bust chance if you hit: %.1f%%\n" (bust_p *. 100.0));
    let dist_str =
      String.concat ", "
        (List.map
           (fun (v, p) -> Printf.sprintf "%d: %.1f%%" v (p *. 100.0))
           dist)
    in
    Buffer.add_string buf
      (Printf.sprintf "  Next-score distribution: %s\n" dist_str);
    Buffer.add_string buf (Printf.sprintf "  %s\n" (Analysis.recommendation st));
    if Game.can_split st then
      Buffer.add_string buf
        (Printf.sprintf "  %s\n" (Analysis.split_recommendation st));
    ANSITerminal.print_string [ Foreground Black; Bold ] (Buffer.contents buf)
  end

let int_of_string_safe s = try Some (int_of_string s) with Failure _ -> None

let max_legal_bet ~player_balance ~dealer_balance =
  let rec loop bet best =
    if bet > player_balance then best
    else if Game.can_bet ~player_balance ~dealer_balance ~bet then
      loop (bet + 1) bet
    else loop (bet + 1) best
  in
  loop 1 0

let rec prompt_bet ~player_balance ~dealer_balance : int option =
  let max_bet = max_legal_bet ~player_balance ~dealer_balance in
  Printf.printf "\nYou have %s chips. Dealer has %s chips.\n"
    (Render.chips_str player_balance)
    (Render.chips_str dealer_balance);
  if max_bet <= 0 then begin
    print_endline
      "No legal bet is possible at the current table — quitting the round.";
    None
  end
  else begin
    Printf.printf "Enter your bet for this round (1-%d), or q to quit: %!"
      max_bet;
    match read_line () with
    | exception End_of_file ->
        print_endline "\nThanks for playing!";
        None
    | s -> (
        match String.lowercase_ascii (String.trim s) with
        | "" ->
            print_endline "Please enter a number, or q to quit.";
            prompt_bet ~player_balance ~dealer_balance
        | "q" | "quit" -> None
        | text -> (
            match int_of_string_safe text with
            | Some bet when Game.can_bet ~player_balance ~dealer_balance ~bet ->
                Some bet
            | Some _ ->
                Printf.printf "Please enter a whole number from 1 to %d.\n"
                  max_bet;
                prompt_bet ~player_balance ~dealer_balance
            | None ->
                print_endline "Please enter a valid number, or q to quit.";
                prompt_bet ~player_balance ~dealer_balance))
  end

let rec prompt_action (st : Game.state) : Game.state =
  print_state st;
  print_analysis st;
  let can_double = Game.can_double_down st in
  let can_split = Game.can_split st in
  let options =
    "h, s"
    ^ (if can_double then ", d" else "")
    ^ (if can_split then ", p" else "")
    ^ ", t"
  in
  Printf.printf "Action [%s]: %!" options;
  match read_line () with
  | exception End_of_file ->
      print_endline "\n(EOF — standing.)";
      player_loop (Game.resolve_after_stand st)
  | s -> (
      match String.lowercase_ascii (String.trim s) with
      | "h" | "hit" -> player_loop (Game.player_hit st)
      | "s" | "stand" -> player_loop (Game.resolve_after_stand st)
      | "d" | "double" | "double down" ->
          if can_double then player_loop (Game.player_double_down st)
          else begin
            print_endline "Doubling down isn't available on this hand.";
            prompt_action st
          end
      | "p" | "split" ->
          if can_split then player_loop (Game.player_split st)
          else begin
            print_endline "Splitting isn't available on this hand.";
            prompt_action st
          end
      | "t" | "toggle" | "hint" ->
          hints_on := not !hints_on;
          Printf.printf "Hints are now %s.\n"
            (if !hints_on then "ON" else "OFF");
          prompt_action st
      | _ ->
          print_endline
            "Please enter h (hit), s (stand), d (double), p (split), or t \
             (toggle hints).";
          prompt_action st)

and player_loop (st : Game.state) : Game.state =
  match Game.phase st with
  | Game.Round_over -> st
  | Game.Dealer_turn -> Game.resolve_after_stand st
  | Game.Player_turn -> prompt_action st

let play_round ~player_balance ~dealer_balance : Game.state option =
  print_endline "\n=== New round ===";
  match prompt_bet ~player_balance ~dealer_balance with
  | None -> None
  | Some bet ->
      let st =
        Game.init_with_balances ~player_balance ~dealer_balance ~bet ()
      in
      let final =
        match Game.phase st with
        | Game.Round_over ->
            print_endline "Round ended on the initial deal.";
            st
        | _ -> player_loop st
      in
      print_state ~hide_dealer:false final;
      (match Game.outcome final with
      | Some oc -> print_endline (Render.outcome oc)
      | None -> print_endline "(round ended with no outcome recorded)");
      Printf.printf "Chip totals: you have %s, dealer has %s.\n"
        (Render.chips_str (Game.player_balance final))
        (Render.chips_str (Game.dealer_balance final));
      Some final

let rec prompt_play_again () : bool =
  Printf.printf "\nPlay another round? [y/n]: %!";
  match read_line () with
  | exception End_of_file ->
      print_endline "";
      false
  | s -> (
      match String.lowercase_ascii (String.trim s) with
      | "y" | "yes" -> true
      | "n" | "no" | "q" | "quit" -> false
      | _ ->
          print_endline "Please answer y (yes) or n (no).";
          prompt_play_again ())

let rec main_loop player_balance dealer_balance : unit =
  if player_balance <= 0 then
    ANSITerminal.print_string [ Foreground Red ] "Game over.\n"
  else if dealer_balance <= 0 then
    ANSITerminal.print_string [ Foreground Green; Bold ]
      "The dealer is out of chips. You win the table!\n"
  else
    match play_round ~player_balance ~dealer_balance with
    | None -> print_endline "Thanks for playing!"
    | Some final ->
        let new_player = Game.player_balance final in
        let new_dealer = Game.dealer_balance final in
        if new_player <= 0 || new_dealer <= 0 then
          main_loop new_player new_dealer
        else if prompt_play_again () then main_loop new_player new_dealer
        else print_endline "Thanks for playing!"

let dealer_initial_balance = 500

let () =
  print_endline "Welcome to Blackjack! (8 decks)";
  Printf.printf "You start with %s chips. The dealer starts with %s chips.\n"
    (Render.chips_str Game.initial_balance)
    (Render.chips_str dealer_initial_balance);
  print_endline "Hints are ON. Press 't' at any prompt to toggle them.";
  main_loop Game.initial_balance dealer_initial_balance
