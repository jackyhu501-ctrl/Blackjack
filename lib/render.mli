(** Frontend-agnostic rendering of game state to strings. No I/O — a CLI, GUI,
    or test suite can all consume these. *)

(** Dealer's hand with only the first card dealt revealed; the rest are shown as
    [[hidden]]. Used during the player's turn. *)
val dealer_hidden : Hand.t -> string

(** A hand rendered as "Card1, Card2, ... (value: N)". *)
val hand_full : Hand.t -> string

(** Multi-line rendering of the full game state. When [hide_dealer] is [true]
    (the default), the dealer's hole card is hidden. *)
val state : ?hide_dealer:bool -> Game.state -> string

(** Human-readable outcome string. Colored via ANSI escape codes. *)
val outcome : Game.outcome -> string

(** A chip count rendered as a yellow-colored number. *)
val chips_str : int -> string
