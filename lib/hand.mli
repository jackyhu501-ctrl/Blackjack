(** A blackjack hand. *)

(** The abstract type of blackjack hands. *)
type t

(** [empty] is a hand with no cards. *)
val empty : t

(** [add_card c h] is the hand formed by adding card [c] to hand [h]. *)
val add_card : Card.card -> t -> t

(** [cards h] is the list of cards in [h]. The most recently drawn card is
    first. *)
val cards : t -> Card.card list

(** [value h] is the blackjack value of [h]. Each Ace counts as 11 unless that
    would make the hand bust, in which case Aces are converted from 11 to 1 one
    at a time until the hand no longer busts or there are no more Aces to
    convert. *)
val value : t -> int

(** [is_bust h] is [true] when [value h > 21]. *)
val is_bust : t -> bool

(** [is_blackjack h] is [true] when [h] has exactly two cards with total value
    21. *)
val is_blackjack : t -> bool

(** [to_string h] is a comma-separated string of the cards in draw order.
    Returns ["(empty hand)"] when [h] has no cards. *)
val to_string : t -> string
