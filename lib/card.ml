type suit =
  | Hearts
  | Diamonds
  | Clubs
  | Spades

type rank =
  | Ace
  | Num of int
  | Jack
  | Queen
  | King

type card = {
  suit : suit;
  rank : rank;
}

let to_int card =
  match card.rank with
  | Ace -> 11
  | Num n -> n
  | Jack | Queen | King -> 10

let string_of_suit = function
  | Hearts -> "Hearts"
  | Diamonds -> "Diamonds"
  | Clubs -> "Clubs"
  | Spades -> "Spades"

let string_of_rank = function
  | Ace -> "Ace"
  | Num n -> string_of_int n
  | Jack -> "Jack"
  | Queen -> "Queen"
  | King -> "King"

let to_string c = string_of_rank c.rank ^ " of " ^ string_of_suit c.suit
