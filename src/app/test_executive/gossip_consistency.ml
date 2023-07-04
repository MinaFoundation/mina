open Core
open Integration_test_lib

module Make (Inputs : Intf.Test.Inputs_intf) = struct
  open Inputs.Dsl
  open Inputs.Engine

  open Test_common.Make (Inputs)

  let test_name = "gossip-consis"

  let config =
    let open Test_config in
    { default with
      requires_graphql = true
    ; genesis_ledger =
        [ { account_name = "node-a-key"; balance = "1000"; timing = Untimed }
        ; { account_name = "node-b-key"; balance = "1000"; timing = Untimed }
        ]
    ; block_producers =
        [ { node_name = "node-a"; account_name = "node-a-key" }
        ; { node_name = "node-b"; account_name = "node-b-key" }
        ]
    }

  let run network t =
    let open Malleable_error.Let_syntax in
    let logger = Logger.create ~prefix:(test_name ^ " test: ") () in
    [%log info] "starting..." ;
    let%bind () = Wait_for.all_nodes_to_initialize t network in
    [%log info] "done waiting for initializations" ;
    let receiver_bp = get_bp_node network "node-a" in
    let%bind receiver_pub_key = pub_key_of_node receiver_bp in
    let sender_bp = get_bp_node network "node-b" in
    let%bind sender_pub_key = pub_key_of_node sender_bp in
    let num_payments = 3 in
    let fee = Currency.Fee.of_nanomina_int_exn 10_000_000 in
    let amount = Currency.Amount.of_nanomina_int_exn 10_000_000 in
    [%log info] "will now send %d payments" num_payments ;
    let%bind hashlist =
      Payment_util.send_n ~logger ~sender_pub_key ~receiver_pub_key
        ~node:sender_bp ~fee ~amount num_payments
    in
    [%log info] "sending payments done. will now wait for payments" ;
    let%bind () =
      Wait_for.payments_to_be_included_in_transition_frontier ~logger ~dsl:t
        ~hashlist num_payments
    in
    [%log info] "finished waiting for payments" ;
    let gossip_states = (network_state t).gossip_received in
    let num_transactions_seen =
      let open Gossip_state in
      let ss =
        Map.data gossip_states
        |> List.map ~f:(Fn.compose By_direction.received transactions)
      in
      Set.(size (union ss))
    in
    [%log info] "num_transactions_seen = %d" num_transactions_seen ;
    let%bind () =
      if num_transactions_seen < num_payments - 1 then (
        let result =
          Malleable_error.soft_error_string ~value:()
            (Printf.sprintf
               "transactions seen = %d, which is less than (numpayments = %d) \
                - 1"
               num_transactions_seen num_payments )
        in
        [%log error]
          "TEST FAILURE.  transactions seen = %d, which is less than \
           (numpayments = %d) - 1"
          num_transactions_seen num_payments ;
        result )
      else
        let result = Malleable_error.ok_unit in
        [%log info] "num_transactions_seen OK" ;
        result
    in
    let `Seen_by_all inter, `Seen_by_some union =
      Gossip_state.stats Transactions_gossip
        (Map.data (network_state t).gossip_received)
        ~exclusion_list:[ Network.Node.id sender_bp ]
    in
    [%log info] "inter = %d; union = %d " inter union ;
    let ratio =
      if union = 0 then 1. else Float.of_int inter /. Float.of_int union
    in
    [%log info] "consistency ratio = %f" ratio ;
    let threshold = 0.95 in
    let%map () =
      if Float.(ratio < threshold) then (
        let result =
          Malleable_error.soft_error_string ~value:()
            (Printf.sprintf
               "consistency ratio = %f, which is less than threshold = %f" ratio
               threshold )
        in
        [%log error]
          "TEST FAILURE. consistency ratio = %f, which is less than threshold \
           = %f"
          ratio threshold ;
        result )
      else
        let result = Malleable_error.ok_unit in
        [%log info] "consistency ratio OK" ;
        result
    in
    [%log info] "finished!"
end
