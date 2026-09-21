open Card

(* AF: the list [c1; ...; cn] is the deck with c1 on top, cn on the bottom. []
   is the empty deck. RI: none. Duplicates are fine -- we use 8 copies of every
   card. *)
type t = Card.card list

let empty = []
let to_list d = d
let size d = List.length d
let all_suits = [ Card.Hearts; Card.Diamonds; Card.Clubs; Card.Spades ]

let all_ranks =
  [
    Card.Ace;
    Card.Num 2;
    Card.Num 3;
    Card.Num 4;
    Card.Num 5;
    Card.Num 6;
    Card.Num 7;
    Card.Num 8;
    Card.Num 9;
    Card.Num 10;
    Card.Jack;
    Card.Queen;
    Card.King;
  ]

(* [n] copies of [x] glued together. *)
let rec replicate n x = if n <= 0 then [] else x @ replicate (n - 1) x

let standard =
  replicate 8
    (List.flatten
       (List.map
          (fun suit -> List.map (fun rank -> { Card.suit; rank }) all_ranks)
          all_suits))

let deal d =
  match d with
  | [] -> None
  | c :: rest -> Some (c, rest)

let shuffle d =
  let arr = Array.of_list d in
  let n = Array.length arr in
  for i = n - 1 downto 1 do
    let j = Random.int (i + 1) in
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp
  done;
  Array.to_list arr

let size (d : t) : int = List.length d
let of_list (lst : card list) : t = lst
