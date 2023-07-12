open Core
open Mina_base
open Mina_transaction
open Unsigned

(** Block producer value represents the incentive a block producer has
    to include a particular transaction in a block. The higher the value,
    the more a block producer should want to include a transaction.
    Conversely, when a block producer has to drop transactions from the
    mempool, they should select those with the lowest value.

    Conceptually it depends on the nonce gap and on the transaction fee.
    The nonce gap is defined as a number of transactions missing in an
    account's queue until the transaction could eventually be applied.
    For example if current account's nonce is 5 and the mempool contains
    transactions with nonces 5, 7, 8 and 11, then transaction with nonce
    5 has a gap of 0, 7 – 1, 8 – also 1, while the transactions with nonce
    11 has a gap of 3. Only transactions with a gap of 0 can potentially
    be applied. Thus, increasing nonce gap decreases the value of a
    transaction. In an account's queue, the nonce gap never decreases as
    nonces increase.

    Because a positive nonce gap excludes the possibility of a transaction
    being included, it is far more important than the fee. Therefore, the fee
    is only taken into account when nonce gaps of two compared transactions
    are equal. *)
type t [@@deriving eq, compare, sexp]

module Map : Map.S with type Key.t = t

val of_transaction :
     nonce_gap:UInt32.t
  -> Transaction_hash.User_command_with_valid_signature.t
  -> t

val of_transaction_queue_empty :
     account:Account.t
  -> Transaction_hash.User_command_with_valid_signature.t
  -> t

val is_applicable : t -> bool
