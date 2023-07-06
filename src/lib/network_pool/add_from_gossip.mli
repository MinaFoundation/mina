open Core
open Mina_base
open Mina_transaction
open Currency

type cmd = Transaction_hash.User_command_with_valid_signature.t

type insertion_result =
  { queue : cmd F_sequence.t
  ; dropped : cmd F_sequence.t
  ; required_balance : Currency.Amount.t
  }

(* These state constructors are revealed for testing purposes.
   There's no reason to use them in production code. *)
module State : sig
  type t =
    < step : cmd -> (t, Command_error.t) Result.t
    ; finalize : (insertion_result, Command_error.t) Result.t
    ; queue : cmd F_sequence.t
    ; dropped : cmd F_sequence.t
    ; required_balance : Amount.t
    ; required_fee : Fee.t option >

  val initial : balance:Amount.t -> cmd -> t

  val inserted :
       balance:Amount.t
    -> ?required_balance:Amount.t
    -> ?replaced:cmd
    -> fee:Fee.t
    -> cmd F_sequence.t
    -> t

  val dropping :
       ?dropped:cmd F_sequence.t
    -> required_balance:Amount.t
    -> ?required_fee:Fee.t
    -> fee:Fee.t
    -> cmd F_sequence.t
    -> t
end

val cmd_nonce : cmd -> Account.Nonce.t

val cmd_fee : cmd -> Fee.t

val insert_into_queue :
     balance:Amount.t
  -> cmd
  -> cmd F_sequence.t
  -> (insertion_result, Command_error.t) Result.t
