open Card

(* AF: [cn; ...; c1] is the hand where c1 was drawn first and cn most recently.
   [] is an empty hand. RI: none. *)
type t = card list

let empty : t = []

let value (h : t) : int =
  let base, aces =
    List.fold_left
      (fun (sum, aces) card ->
        match card.rank with
        | Ace -> (sum + 11, aces + 1)
        | _ -> (sum + to_int card, aces))
      (0, 0) h
  in
  let rec demote total remaining =
    if total <= 21 || remaining = 0 then total
    else demote (total - 10) (remaining - 1)
  in
  demote base aces

let add_card (c : card) (h : t) : t = c :: h
let is_bust (h : t) : bool = value h > 21
let is_blackjack (h : t) : bool = List.length h = 2 && value h = 21
let cards (h : t) : Card.card list = h

let to_string (h : t) : string =
  match h with
  | [] -> "(empty hand)"
  | _ -> h |> List.rev |> List.map Card.to_string |> String.concat ", "
