(** A playing card. *)

(** The four suits in a standard deck. *)
type suit =
  | Hearts
  | Diamonds
  | Clubs
  | Spades

(** Rank of a card. [Num n] is a number card; the intended precondition is
    [2 <= n <= 10]. *)
type rank =
  | Ace
  | Num of int
  | Jack
  | Queen
  | King

(** A standard playing card with a [suit] and [rank]. *)
type card = {
  suit : suit;
  rank : rank;
}

(** [to_int c] is the blackjack point value of [c]. Face cards are worth 10,
    number cards are worth their number, and an Ace is worth 11. *)
val to_int : card -> int

(** [to_string c] is a human-readable string for [c], such as ["Ace of Hearts"]
    or ["7 of Clubs"]. *)
val to_string : card -> string
