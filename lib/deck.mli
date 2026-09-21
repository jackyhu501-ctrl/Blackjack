(** A deck, or shoe, of cards. The card dealt next is considered the top card.
*)

(** The abstract type of decks. *)
type t

(** [empty] is a deck with no cards. *)
val empty : t

(** [standard] is an 8-deck blackjack shoe with 416 cards in a fixed order.
    Shuffle before using it in normal gameplay. *)
val standard : t

(** [shuffle d] is a shuffled version of [d]. The caller is responsible for
    seeding [Random] before calling this function. *)
val shuffle : t -> t

(** [deal d] removes and returns the top card of [d]. It returns [None] if [d]
    is empty. *)
val deal : t -> (Card.card * t) option

(** [size d] is the number of cards remaining in [d]. *)
val size : t -> int

(** [to_list d] is the list of cards in [d], with the top card first. *)
val to_list : t -> Card.card list

(** [of_list cards] is a deck containing exactly [cards] in the given order. The
    first card in the list is the top card. This is mainly useful for
    deterministic tests. *)
val of_list : Card.card list -> t
