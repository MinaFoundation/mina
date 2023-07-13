open Core_kernel
open Integration_test_lib

module Make (Inputs : Intf.Test.Inputs_intf) = struct
  open Inputs.Dsl
  open Inputs.Engine

  open Test_common.Make (Inputs)

  let test_name = "delegation"

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
    ; num_archive_nodes = 1
    }

  let run network t =
    let open Malleable_error.Let_syntax in
    let logger = Logger.create ~prefix:(test_name ^ "test: ") () in
    let%bind () = Wait_for.all_nodes_to_initialize t network in
    let node_a = get_bp_node network "node-a" in
    let node_b = get_bp_node network "node-b" in
    let%bind () =
      section "Delegate all tokens from node_b to node_a"
        (let%bind delegation_receiver_pub_key = pub_key_of_node node_a in
         let%bind delegation_sender_pub_key = pub_key_of_node node_b in
         let fee = Currency.Fee.of_nanomina_int_exn 10_000_000 in
         let%bind { hash; _ } =
           Network.Node.must_send_delegation ~logger node_b
             ~sender_pub_key:delegation_sender_pub_key
             ~receiver_pub_key:delegation_receiver_pub_key ~fee
         in
         Wait_for.signed_command_to_be_included_in_frontier t ~txn_hash:hash
           ~node_included_in:`Any_node )
    in
    section_hard "Running replayer"
      (Archive_node.run_and_check_replayer ~logger network)
end
