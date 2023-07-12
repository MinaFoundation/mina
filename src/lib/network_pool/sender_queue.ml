open Core
open Currency
open Mina_base
open Mina_numbers
open Mina_transaction

type cmd = Transaction_hash.User_command_with_valid_signature.t

type insertion_result =
  { queue : cmd F_sequence.t
  ; dropped : cmd F_sequence.t
  ; required_balance : Amount.t
  }

let unless_overflows = Result.of_option ~error:Command_error.Overflow

let consumed_currency_unchecked :
    User_command.t -> (Amount.t, Command_error.t) Result.t =
 fun cmd ->
  let fee_amt = Currency.Amount.of_fee @@ User_command.fee cmd in
  let open Currency.Amount in
  let amt =
    match cmd with
    | Signed_command c -> (
        match c.payload.body with
        | Payment { amount; _ } ->
            (* The fee-payer is also the sender account, include the amount. *)
            amount
        | Stake_delegation _ ->
            zero )
    | Zkapp_command _ ->
        (*TODO: document- txns succeeds with source amount insufficient in the case of zkapps*)
        zero
  in
  unless_overflows Amount.(amt + fee_amt)

let consumed_currency cmd =
  Transaction_hash.User_command_with_valid_signature.command cmd
  |> consumed_currency_unchecked

let cmd_nonce cmd =
  Transaction_hash.User_command_with_valid_signature.command cmd
  |> User_command.applicable_at_nonce

let cmd_fee cmd =
  Transaction_hash.User_command_with_valid_signature.command cmd
  |> User_command.fee

module State = struct
  type t =
    < step : cmd -> (t, Command_error.t) Result.t
    ; finalize : (insertion_result, Command_error.t) Result.t
    ; queue : cmd F_sequence.t
    ; dropped : cmd F_sequence.t
    ; required_balance : Amount.t
    ; required_fee : Fee.t option >

  class balance_tracker ?(required_balance = Amount.zero) available_balance =
    object (self)
      val available_balance : Amount.t = available_balance

      val required_balance : Amount.t = required_balance

      method remaining : (Amount.t, Command_error.t) Result.t =
        unless_overflows Amount.(available_balance - required_balance)

      method required_balance = required_balance

      method add_required_balance (amount : Amount.t) =
        let open Result.Let_syntax in
        let%map required =
          unless_overflows Amount.(required_balance + amount)
        in
        {<available_balance; required_balance = required>}

      method balance_sufficient = Amount.(available_balance >= required_balance)

      method assert_balance =
        Result.ok_if_true
          ~error:
            (Command_error.Insufficient_funds
               (`Balance available_balance, required_balance) )
          self#balance_sufficient
    end

  class queue initial =
    object
      val q : cmd F_sequence.t = initial

      method append (cmd : cmd) = {<q = F_sequence.snoc q cmd>}

      method empty = F_sequence.length q = 0

      method queue = q
    end

  class replacement_fee_tracker ?required_fee replacement_fee =
    object
      val replacement_fee : Fee.t = replacement_fee

      val required_fee : Fee.t option = required_fee

      method required_fee = required_fee

      method add_required_fee (fee : Fee.t) =
        match required_fee with
        | None ->
            Result.return fee
        | Some required ->
            unless_overflows Fee.(required + fee)

      method assert_required_fee : (unit, Command_error.t) Result.t =
        match required_fee with
        | Some required ->
            Result.ok_if_true
              ~error:
                (Command_error.Insufficient_replace_fee
                   (`Replace_fee replacement_fee, required) )
              Fee.(replacement_fee > required)
        | None ->
            Result.return ()
    end

  class dropping ?(dropped = F_sequence.empty) ~required_balance ?required_fee
    ~fee q =
    object (self)
      inherit queue q

      inherit replacement_fee_tracker ?required_fee fee

      val dropped : cmd F_sequence.t = dropped

      val required_balance : Amount.t = required_balance

      method required_balance = required_balance

      method dropped = dropped

      method step (cmd : cmd) =
        let open Result.Let_syntax in
        let%map required = self#add_required_fee (cmd_fee cmd) in
        ( {< dropped = F_sequence.snoc dropped cmd
           ; replacement_fee
           ; required_balance
           ; required_fee = Some required >}
          :> t )

      method finalize : (insertion_result, Command_error.t) Result.t =
        let open Result.Let_syntax in
        let%map () = self#assert_required_fee in
        { queue = q; dropped; required_balance }
    end

  class inserted ~balance ?required_balance ?replaced ~fee q =
    object (self)
      inherit queue q

      inherit balance_tracker balance ?required_balance

      inherit
        replacement_fee_tracker
          ?required_fee:Option.(map ~f:cmd_fee replaced)
          fee

      method dropped =
        Option.value_map ~default:F_sequence.empty ~f:F_sequence.singleton
          replaced

      method step (cmd : cmd) =
        let open Result.Let_syntax in
        let%bind consumed = consumed_currency cmd in
        let%bind updated = self#add_required_balance consumed in
        if updated#balance_sufficient then return (updated#append cmd :> t)
        else
          let state =
            new dropping
              ~dropped:updated#dropped
                (* Don't update the required balance, because we're dropping the current command. *)
              ~required_balance ?required_fee:updated#required_fee ~fee q
          in
          state#step cmd

      method finalize : (insertion_result, Command_error.t) Result.t =
        let open Result.Let_syntax in
        let%map () = self#assert_required_fee in
        { queue = q; dropped = self#dropped; required_balance }
    end

  class initial ~balance to_insert =
    object (self)
      inherit queue F_sequence.empty

      inherit balance_tracker balance

      val nonce : Account_nonce.t = cmd_nonce to_insert

      val to_insert : cmd = to_insert

      method required_fee = None

      method dropped = F_sequence.empty

      method transition ?replaced () =
        let open Result.Let_syntax in
        let state =
          new inserted
            ~balance:available_balance ~required_balance
            ~fee:(cmd_fee to_insert) ?replaced q
        in
        let%bind consumed = consumed_currency to_insert in
        let%bind state' = state#add_required_balance consumed in
        let%map () = state'#assert_balance in
        state'

      method step (cmd : cmd) =
        let open Result.Let_syntax in
        match Account_nonce.compare (cmd_nonce cmd) nonce with
        | -1 ->
            let%bind consumed = consumed_currency cmd in
            let%bind updated = self#add_required_balance consumed in
            let%map () = updated#assert_balance in
            (updated#append cmd :> t)
        | 0 ->
            let%map state = self#transition ~replaced:cmd () in
            (state#append to_insert :> t)
        | _ ->
            let%bind state = self#transition () in
            let%bind consumed = consumed_currency cmd in
            let%bind state' = state#add_required_balance consumed in
            let%map () = state'#assert_balance in
            ((state'#append to_insert)#append cmd :> t)

      method finalize : (insertion_result, Command_error.t) Result.t =
        let open Result.Let_syntax in
        let%bind consumed = consumed_currency to_insert in
        let%bind updated = self#add_required_balance consumed in
        let%map () = updated#assert_balance in
        let final = updated#append to_insert in
        { queue = final#queue
        ; dropped = F_sequence.empty
        ; required_balance = final#required_balance
        }
    end

  let initial ~balance cmd = (new initial ~balance cmd :> t)

  let inserted ~balance ?required_balance ?replaced ~fee q =
    (new inserted ~balance ?required_balance ?replaced ~fee q :> t)

  let dropping ?(dropped = F_sequence.empty) ~required_balance ?required_fee
      ~fee q =
    (new dropping ~dropped ~required_balance ?required_fee ~fee q :> t)
end

let fseq_foldl_result ~(init : State.t) seq :
    (insertion_result, Command_error.t) Result.t =
  let open Result.Let_syntax in
  let rec go state seq =
    match F_sequence.uncons seq with
    | None ->
        state#finalize
    | Some (x, seq) ->
        let%bind state' = state#step x in
        go state' seq
  in
  go init seq

let insert_into_queue ~balance cmd queue =
  fseq_foldl_result queue ~init:(State.initial ~balance cmd)
