module Make : functor
  (Inputs : Integration_test_lib.Intf.Test.Inputs_intf)
  -> sig
  (** [repeat_seq ~n ~f] calls the function [f] [n] times in sequence *)
  val repeat_seq :
       n:int
    -> f:(unit -> unit Integration_test_lib.Malleable_error.t)
    -> unit Integration_test_lib.Malleable_error.t

  (** TODO: [send_payments] *)
  val send_payments :
       logger:Logger.t
    -> sender_pub_key:Signature_lib.Public_key.Compressed.t
    -> receiver_pub_key:Signature_lib.Public_key.Compressed.t
    -> amount:Currency.Amount.t
    -> fee:Currency.Fee.t
    -> node:Inputs.Engine.Network.Node.t
    -> int
    -> Mina_transaction.Transaction_hash.t list
       Integration_test_lib.Malleable_error.t

  module Wait_for : sig
    (** [all_nodes_to_initialize dsl network] uses [dsl]'s wait condition to wait for all nodes in the [network] to initialize *)
    val all_nodes_to_initialize :
         Inputs.Dsl.t
      -> Inputs.Engine.Network.t
      -> unit Integration_test_lib.Malleable_error.t

    (** [node_to_initialize dsl node] uses [dsl]'s wait condition to wait for [node] to initialize *)
    val node_to_initialize :
         Inputs.Dsl.t
      -> Inputs.Engine.Network.Node.t
      -> unit Integration_test_lib.Malleable_error.t

    (** [blocks_to_be_produced dsl n] uses [dsl]'s wait condition to wait for [n] blocks *)
    val blocks_to_be_produced :
      Inputs.Dsl.t -> int -> unit Integration_test_lib.Malleable_error.t

    (** [nodes_to_synchronize dsl nodes] uses [dsl]'s wait condition to wait for [nodes] to synchronize with each other *)
    val nodes_to_synchronize :
         Inputs.Dsl.t
      -> Inputs.Engine.Network.Node.t list
      -> unit Integration_test_lib.Malleable_error.t

    (** TODO: [ledger_proofs_emitted_since_genesis ~test_config ~num_proofs]  *)
    val ledger_proofs_emitted_since_genesis :
         Inputs.Dsl.t
      -> test_config:Integration_test_lib.Test_config.t
      -> num_proofs:int
      -> unit Integration_test_lib.Malleable_error.t

    (** TODO: [payments_to_be_included_in_transition_frontier ~logger ~dsl ~hashlist n] *)
    val payments_to_be_included_in_transition_frontier :
         logger:Logger.t
      -> dsl:Inputs.Dsl.t
      -> hashlist:Mina_transaction.Transaction_hash.t list
      -> int
      -> unit Integration_test_lib.Malleable_error.t

    (** TODO: [signed_command_to_be_included_in_frontier dsl ~txn_hash ~node_included_in] *)
    val signed_command_to_be_included_in_frontier :
         Inputs.Dsl.t
      -> txn_hash:Mina_transaction.Transaction_hash.t
      -> node_included_in:[ `Any_node | `Node of Inputs.Engine.Network.Node.t ]
      -> unit Integration_test_lib.Malleable_error.t

    (** TODO: [with_timeouts dsl ~condition ~soft_timeout ~hard_timeout] *)
    val with_timeouts :
         Inputs.Dsl.t
      -> condition:Inputs.Dsl.Wait_condition.t
      -> soft_timeout:Integration_test_lib.Network_time_span.t
      -> hard_timeout:Integration_test_lib.Network_time_span.t
      -> unit Integration_test_lib.Malleable_error.t

    (** [zkapp_to_be_included_in_frontier] *)
    val zkapp_to_be_included_in_frontier :
         Inputs.Dsl.t
      -> has_failures:bool
      -> zkapp_command:Mina_base.Zkapp_command.t
      -> soft_slots:int
      -> unit Integration_test_lib.Malleable_error.t
  end

  (** [get_node network node_name] returns the node in the [network] with the given [node_name] *)
  val get_node :
    Inputs.Engine.Network.t -> string -> Inputs.Engine.Network.Node.t

  (** [get_bp_node network node_name] returns the block producer node in the [network] with the given [node_name] *)
  val get_bp_node :
    Inputs.Engine.Network.t -> string -> Inputs.Engine.Network.Node.t

  (** [get_genesis_keypair network account_name] returns the keypair from the genesis account with the given G[account_name] *)
  val get_genesis_keypair :
    Inputs.Engine.Network.t -> string -> Integration_test_lib.Network_keypair.t

  (** [pub_key_of_node node] returns the compressed public key of the node *)
  val pub_key_of_node :
       Inputs.Engine.Network.Node.t
    -> Signature_lib.Public_key.Compressed.t
       Integration_test_lib.Malleable_error.t

  (** [priv_key_of_node node] returns the private key of the node *)
  val priv_key_of_node :
       Inputs.Engine.Network.Node.t
    -> Signature_lib.Private_key.t Integration_test_lib.Malleable_error.t

  (** TODO: [check_common_prefixes ~tolerance ~logger ...]*)
  val check_common_prefixes :
       tolerance:int
    -> logger:Logger.t
    -> string list list
    -> unit Integration_test_lib.Malleable_error.t

  (** TODO: [fetch_connectivity_data ~logger nodes]*)
  val fetch_connectivity_data :
       logger:Logger.t
    -> Inputs.Engine.Network.Node.t list
    -> (Inputs.Engine.Network.Node.t * (string * string list)) list
       Integration_test_lib.Malleable_error.t

  (** TODO: [assert_peers_completely_connected ...]*)
  val assert_peers_completely_connected :
       (Inputs.Engine.Network.Node.t * (string * string list)) list
    -> unit Integration_test_lib.Malleable_error.t

  (** TODO: [assert_peers_cant_be_partitioned ~max_disconnection ...]*)
  val assert_peers_cant_be_partitioned :
       max_disconnections:int
    -> ('a * (string * string list)) list
    -> ( unit Integration_test_lib.Malleable_error.Result_accumulator.t
       , Integration_test_lib.Malleable_error.Hard_fail.t )
       result
       Async.Deferred.t

  module Zkapp : sig
    (** TODO: [send_batch ~logger node zkapp_command]*)
    val send_batch :
         logger:Logger.t
      -> Inputs.Engine.Network.Node.t
      -> Mina_base.Zkapp_command.t list
      -> ( unit Integration_test_lib.Malleable_error.Result_accumulator.t
         , Integration_test_lib.Malleable_error.Hard_fail.t )
         result
         Async.Deferred.t

    (** TODO: [send ~logger node zkapp_command]*)
    val send :
         logger:Logger.t
      -> Inputs.Engine.Network.Node.t
      -> Mina_base.Zkapp_command.t
      -> ( unit Integration_test_lib.Malleable_error.Result_accumulator.t
         , Integration_test_lib.Malleable_error.Hard_fail.t )
         result
         Async.Deferred.t

    (** TODO: [send_invalid ~logger node zkapp_command ...]*)
    val send_invalid :
         logger:Logger.t
      -> Inputs.Engine.Network.Node.t
      -> Mina_base.Zkapp_command.t
      -> string
      -> ( unit Integration_test_lib.Malleable_error.Result_accumulator.t
         , Integration_test_lib.Malleable_error.Hard_fail.t )
         result
         Async.Deferred.t

    (** TODO: [send_invalid_payment ~logger node ~sender_pub_key ~receiver_pub_key ~amount ~fee ~nonce ~memo ~valid_until ~raw_signature ~expected_failure]*)
    val send_invalid_payment :
         logger:Logger.t
      -> Inputs.Engine.Network.Node.t
      -> sender_pub_key:Signature_lib.Public_key.Compressed.t
      -> receiver_pub_key:Signature_lib.Public_key.Compressed.t
      -> amount:Currency.Amount.t
      -> fee:Currency.Fee.t
      -> nonce:Unsigned.uint32
      -> memo:string
      -> valid_until:Mina_numbers.Global_slot_since_genesis.t
      -> raw_signature:string
      -> expected_failure:string
      -> unit Integration_test_lib.Malleable_error.t

    (** TODO: [get_account_permissions ~logger node account_id]*)
    val get_account_permissions :
         logger:Logger.t
      -> Inputs.Engine.Network.Node.t
      -> Mina_base.Account_id.t
      -> ( Mina_base.Permissions.t
           Integration_test_lib.Malleable_error.Result_accumulator.t
         , Integration_test_lib.Malleable_error.Hard_fail.t )
         result
         Async.Deferred.t

    (** TODO: [get_account_update ~logger node account_id]*)
    val get_account_update :
         logger:Logger.t
      -> Inputs.Engine.Network.Node.t
      -> Mina_base.Account_id.t
      -> ( Mina_base.Account_update.Update.t
           Integration_test_lib.Malleable_error.Result_accumulator.t
         , Integration_test_lib.Malleable_error.Hard_fail.t )
         result
         Async.Deferred.t

    (** TODO: [compatible_item ... ~equal]*)
    val compatible_item :
         'a Mina_base.Zkapp_basic.Set_or_keep.t
      -> 'b Mina_base.Zkapp_basic.Set_or_keep.t
      -> equal:('a -> 'b -> bool)
      -> bool

    (** TODO: [compatible_updates ~ledger_update ~requested_update]*)
    val compatible_updates :
         ledger_update:Mina_base.Account_update.Update.t
      -> requested_update:Mina_base.Account_update.Update.t
      -> bool
  end

  (** TODO: [check_replayer_logs ~logger ...]*)
  val check_replayer_logs :
       logger:Logger.t
    -> string
    -> ( unit Integration_test_lib.Malleable_error.Result_accumulator.t
       , Integration_test_lib.Malleable_error.Hard_fail.t )
       result
       Async.Deferred.t
end
