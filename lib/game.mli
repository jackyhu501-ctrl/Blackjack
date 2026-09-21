(** One round of blackjack, including chip balances, betting, splitting, and
    doubling down. *)

(** Whose turn it is, or whether the round is finished. *)
type phase =
  | Player_turn
  | Dealer_turn
  | Round_over

(** Result from the player's point of view. [Player_blackjack] means the player
    got a natural blackjack from the initial two-card deal, so the bet pays 3:2.
*)
type outcome =
  | Player_win
  | Player_blackjack
  | Dealer_win
  | Push

(** Snapshot of the round. [outcome] is [Some _] iff [phase = Round_over].

    [player_balance] and [dealer_balance] are the chip totals after any
    completed settlement, or the current chip totals while the round is active.
    [current_bet] is the main hand's wager for the round.

    [player_hands], [hand_bets], [hand_done], and [hand_outcomes] are parallel
    lists used to support splitting. In a normal round these lists have one
    element. After a split, they contain one element per player hand.
    [active_hand] is the index of the hand currently being played. *)
type state = {
  player_hand : Hand.t;
  dealer_hand : Hand.t;
  deck : Deck.t;
  phase : phase;
  outcome : outcome option;
  player_balance : int;
  dealer_balance : int;
  current_bet : int;
  player_hands : Hand.t list;
  active_hand : int;
  hand_bets : int list;
  hand_done : bool list;
  hand_outcomes : outcome option list;
}

(** Each side starts the game with this many chips. *)
val initial_balance : int

(** [can_bet ~player_balance ~dealer_balance ~bet] is [true] exactly when [bet]
    is positive, the player can afford it, and the dealer can cover the largest
    possible payout for that bet: a 3:2 player blackjack. *)
val can_bet : player_balance:int -> dealer_balance:int -> bet:int -> bool

(** [init ()] starts a fresh round using the standard starting balances and a
    bet of one chip. This is kept for compatibility with older code. *)
val init : unit -> state

(** [init_with_bet bet] starts a fresh round using the standard starting
    balances and the given [bet]. Raises [Invalid_argument] if [bet] is not
    valid. *)
val init_with_bet : int -> state

(** [init_with_balances ~player_balance ~dealer_balance ~bet ()] starts a fresh
    round with custom balances and bet. Raises [Invalid_argument] if the bet is
    not positive or either side cannot cover it. *)
val init_with_balances :
  player_balance:int -> dealer_balance:int -> bet:int -> unit -> state

(** [init_with_deck ~player_balance ~dealer_balance ~bet deck] is the same as
    [init_with_balances], but uses [deck] without shuffling. The first card in
    [deck] is dealt first. This is mainly useful for deterministic tests. *)
val init_with_deck :
  player_balance:int -> dealer_balance:int -> bet:int -> Deck.t -> state

(** [player_hit st] draws one card for the active player hand. If the hand
    busts, that hand is settled immediately. Otherwise the round stays in
    [Player_turn]. This is a no-op unless [phase st = Player_turn]. *)
val player_hit : state -> state

(** [resolve_after_stand st] stands on the active player hand. If there are more
    split hands to play, the game moves to the next hand. Otherwise the dealer
    plays using the fixed rule of hitting below 17, then all unsettled hands are
    scored and settled. This is a no-op if the round is already over. *)
val resolve_after_stand : state -> state

(** [player_hand st] is the active player's hand. *)
val player_hand : state -> Hand.t

(** [dealer_hand st] is the dealer's hand. *)
val dealer_hand : state -> Hand.t

(** [deck st] is the undealt shoe. *)
val deck : state -> Deck.t

(** [phase st] is the current phase of the round. *)
val phase : state -> phase

(** [outcome st] is [Some o] once the round is decided and [None] while the
    round is still active. *)
val outcome : state -> outcome option

(** [player_balance st] is the player's current chip balance. *)
val player_balance : state -> int

(** [dealer_balance st] is the dealer's current chip balance. *)
val dealer_balance : state -> int

(** [current_bet st] is the bet for the main hand in the current round. *)
val current_bet : state -> int

(** [can_split st] is [true] when the active player hand has exactly two cards
    with the same rank, and both the player and dealer can cover the extra
    matching bet. *)
val can_split : state -> bool

(** [player_split st] splits the active hand into two hands. Each new hand gets
    one extra card. This is a no-op if splitting is not allowed. *)
val player_split : state -> state

(** [can_double_down st] is [true] when the active hand has exactly two cards
    and both sides can cover doubling the current bet. *)
val can_double_down : state -> bool

(** [player_double_down st] doubles the active hand's bet, draws exactly one
    card, then automatically stands on that hand. This is a no-op if doubling
    down is not allowed. *)
val player_double_down : state -> state

(** [player_hands st] is the list of all player hands in the current round.
    Usually this list has one hand, or two hands after a split. *)
val player_hands : state -> Hand.t list

(** [active_hand st] is the index of the player hand currently being played. *)
val active_hand : state -> int

(** [hand_bets st] is the list of bets corresponding to [player_hands st]. *)
val hand_bets : state -> int list

(** [hand_outcomes st] is the list of outcomes corresponding to
    [player_hands st]. A [None] value means that hand has not been settled yet.
*)
val hand_outcomes : state -> outcome option list
