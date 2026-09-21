(** Analysis helpers for blackjack decisions. These functions answer simple
    "what if I hit?" questions by assuming every remaining card in the shoe is
    equally likely to be drawn next. *)

(** The game state analyzed by this module. *)
type state = Game.state

(** [hit_bust_probability st] is the probability that the active player hand
    will bust if the player hits once. Requires that the deck in [st] is
    non-empty. *)
val hit_bust_probability : state -> float

(** [next_scores_distribution st] is the distribution over the active player's
    next [Hand.value] if the player hits once. Each [(v, p)] means that the hand
    becomes value [v] with probability [p]. The result is sorted by score in
    ascending order, and the probabilities sum to 1. Requires that the deck in
    [st] is non-empty. *)
val next_scores_distribution : state -> (int * float) list

(** [recommendation st] is a one-line hit-or-stand suggestion. The strategy
    recommends standing when the bust probability is at least 0.5, and hitting
    otherwise. *)
val recommendation : state -> string

(** [split_recommendation st] is a one-line suggestion about whether splitting
    is currently available or strategically useful. *)
val split_recommendation : state -> string
