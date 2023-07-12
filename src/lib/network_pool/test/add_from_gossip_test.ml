(** Testing
    -------
    Component:  Network pool
    Invocation: dune exec src/lib/network_pool/test/main.exe -- \
                  test '^add from gossip$'
    Subject:    Test adding transactions from gossip.
 *)

open Core_kernel
open Currency
open Mina_base
open Mina_numbers
open Mina_transaction
open Network_pool
open Transaction_gen
open Sender_queue

let currency_consumed =
  Indexed_pool.For_tests.currency_consumed ~constraint_constants

let add_from_gossip_reserves_proper_amount_of_currency () =
  Quickcheck.test
    (let open Quickcheck.Generator.Let_syntax in
    let%bind sender = Account.gen in
    let%bind receiver = Account.gen in
    let%map txns =
      Stateful_gen.eval_state
        (gen_txns_from_single_sender_to receiver.public_key)
        sender
    in
    (sender, txns))
    ~f:(fun (sender, txns) ->
      let module Result_ext = Monad_lib.Make_ext2 (Result) in
      let balance = Balance.to_amount sender.balance in
      let result =
        Result_ext.fold_m
          (List.map
             ~f:(fun t ->
               Transaction_hash.User_command_with_valid_signature.create
                 (Signed_command t) )
             txns )
          ~init:(F_sequence.empty, Amount.zero)
          ~f:(fun (queue, reserved_currency) txn ->
            let open Result.Let_syntax in
            let%bind { queue; dropped; required_balance; nonce_gap } =
              insert_into_queue ~balance ~current_nonce:sender.nonce txn queue
            in
            Result.of_option ~error:Command_error.Overflow
            @@
            let open Option.Let_syntax in
            let open Amount in
            let module Opt_ext = Monad_lib.Make_ext (Option) in
            let%bind consumed = currency_consumed txn in
            let%bind dropped_balance =
              Opt_ext.fold_m (F_sequence.to_list dropped) ~init:zero
                ~f:(fun acc cmd ->
                  let%bind cost = currency_consumed cmd in
                  Currency.Amount.add cost acc )
            in
            let%bind reserve_plus_consumed = add reserved_currency consumed in
            let%map reserve_minus_dropped =
              sub reserve_plus_consumed dropped_balance
            in
            (* Commands are inserted in order, no gaps. *)
            [%test_eq: Account_nonce.t] nonce_gap Unsigned.UInt32.zero ;
            [%test_eq: Amount.t] reserve_minus_dropped required_balance ;
            (queue, required_balance) )
      in
      assert (Result.is_ok result) )
