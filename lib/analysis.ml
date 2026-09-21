open Hand

type state = Game.state

let hit_bust_probability st =
  let bust_counter =
    List.fold_left
      (fun acc card ->
        if Hand.is_bust (Hand.add_card card (Game.player_hand st)) then acc + 1
        else acc)
      0
      (Deck.to_list (Game.deck st))
  in
  float_of_int bust_counter
  /. float_of_int (List.length (Deck.to_list (Game.deck st)))

(* Bump the count for [score] in an assoc list, adding it if new. *)
let rec helper lst score =
  match lst with
  | [] -> [ (score, 1) ]
  | (s, n) :: remaining ->
      if s = score then (s, n + 1) :: remaining
      else (s, n) :: helper remaining score

let next_scores_distribution st =
  let amounts =
    List.fold_left
      (fun acc card ->
        let score = Hand.value (Hand.add_card card (Game.player_hand st)) in
        helper acc score)
      []
      (Deck.to_list (Game.deck st))
  in
  List.sort compare
    (List.map
       (fun (value, amount) ->
         ( value,
           float_of_int amount
           /. float_of_int (List.length (Deck.to_list (Game.deck st))) ))
       amounts)

let recommendation st =
  let player_current_score = Hand.value (Game.player_hand st) in
  let dealer_revealed_score =
    match Hand.cards (Game.dealer_hand st) with
    | card :: _ -> Card.to_int card
    | [] -> failwith "Error: dealer has no cards"
  in

  match player_current_score with
  | n when n >= 17 -> "The optimal move is to stick."
  | n when n > 12 && n <= 16 ->
      if dealer_revealed_score >= 2 && dealer_revealed_score <= 6 then
        "The optimal move is to stick."
      else "The optimal move is to hit."
  | 12 ->
      if dealer_revealed_score >= 4 && dealer_revealed_score <= 6 then
        "The optimal move is to stick."
      else "The optimal move is to hit."
  | 11 ->
      if Game.can_double_down st then "The optimal move is to double down."
      else "The optimal move is to hit."
  | 10 ->
      if dealer_revealed_score >= 2 && dealer_revealed_score <= 9 then
        if Game.can_double_down st then "The optimal move is to double down."
        else "The optimal move is to hit."
      else "The optimal move is to hit."
  | 9 ->
      if dealer_revealed_score >= 3 && dealer_revealed_score <= 6 then
        if Game.can_double_down st then "The optimal move is to double down."
        else "The optimal move is to hit."
      else "The optimal move is to hit."
  | _ -> "The optimal move is to hit."

let split_recommendation st =
  if not (Game.can_split st) then "Splitting is not available."
  else
    let pair_value =
      match Hand.cards (Game.player_hand st) with
      | card :: _ -> Card.to_int card
      | [] -> failwith "Player has no cards"
    in
    let dealer_revealed_score =
      match Hand.cards (Game.dealer_hand st) with
      | card :: _ -> Card.to_int card
      | [] -> failwith "Dealer has no cards"
    in

    match pair_value with
    | 11 | 1 -> "The optimal move is to split."
    | 8 -> "The optimal move is to split."
    | 10 -> "The optimal move is to not split."
    | 9 ->
        if
          (dealer_revealed_score >= 2 && dealer_revealed_score <= 6)
          || dealer_revealed_score = 8 || dealer_revealed_score = 9
        then "The optimal move is to split."
        else "The optimal move is to not split."
    | 7 ->
        if dealer_revealed_score >= 2 && dealer_revealed_score <= 7 then
          "The optimal move is to split."
        else "The optimal move is to not split."
    | 6 ->
        if dealer_revealed_score >= 2 && dealer_revealed_score <= 6 then
          "The optimal move is to split."
        else "The optimal move is to not split."
    | 5 -> "The optimal move is to not split."
    | 4 ->
        if dealer_revealed_score = 5 || dealer_revealed_score = 6 then
          "The optimal move is to split."
        else "The optimal move is to not split."
    | 3 | 2 ->
        if dealer_revealed_score >= 2 && dealer_revealed_score <= 7 then
          "The optimal move is to split."
        else "The optimal move is to not split."
    | _ -> "Splitting is not available."
