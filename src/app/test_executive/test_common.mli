module Make : functor
  (Inputs : Integration_test_lib.Intf.Test.Inputs_intf)
  -> sig
  (** [repeat_seq ~n ~f] calls the function [f] [n] times in sequence *)
  val repeat_seq :
       n:int
    -> f:(unit -> unit Integration_test_lib.Malleable_error.t)
    -> unit Integration_test_lib.Malleable_error.t

  module Payment_util : sig
    (** [send_n ~logger ~sender_pub_key ~receiver_pub_key ~amount ~fee ~node n]
          [node] attempts to send [n] payments of [amount] from [sender_pub_key] to
          [receiver_pub_key] paying [fee] for each, logs progress to [logger] *)
    val send_n :
         logger:Logger.t
      -> sender_pub_key:Signature_lib.Public_key.Compressed.t
      -> receiver_pub_key:Signature_lib.Public_key.Compressed.t
      -> amount:Currency.Amount.t
      -> fee:Currency.Fee.t
      -> node:Inputs.Engine.Network.Node.t
      -> int
      -> Mina_transaction.Transaction_hash.t list
         Integration_test_lib.Malleable_error.t

    (** TODO: [send_invalid ~logger node ~sender_pub_key ~receiver_pub_key ~amount ~fee ~nonce ~memo ~valid_until ~raw_signature ~expected_failure]*)
    val send_invalid :
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
  end

  module Account_util : sig
    (** TODO: [get_permissions ~logger node account_id]*)
    val get_permissions :
         logger:Logger.t
      -> Inputs.Engine.Network.Node.t
      -> Mina_base.Account_id.t
      -> ( Mina_base.Permissions.t
           Integration_test_lib.Malleable_error.Result_accumulator.t
         , Integration_test_lib.Malleable_error.Hard_fail.t )
         result
         Async.Deferred.t

    (** TODO: [get_update ~logger node account_id]*)
    val get_update :
         logger:Logger.t
      -> Inputs.Engine.Network.Node.t
      -> Mina_base.Account_id.t
      -> ( Mina_base.Account_update.Update.t
           Integration_test_lib.Malleable_error.Result_accumulator.t
         , Integration_test_lib.Malleable_error.Hard_fail.t )
         result
         Async.Deferred.t
  end

  module Wait_for : sig
    (** [nodes_to_initialize dsl nodes] uses [dsl]'s wait condition to wait for [nodes] to initialize *)
    val nodes_to_initialize :
         Inputs.Dsl.t
      -> Inputs.Engine.Network.Node.t list
      -> unit Integration_test_lib.Malleable_error.t

    (** [all_nodes_to_initialize dsl network] uses [dsl]'s wait condition to wait for all nodes in the [network] to initialize *)
    val all_nodes_to_initialize :
         Inputs.Dsl.t
      -> Inputs.Engine.Network.t
      -> unit Integration_test_lib.Malleable_error.t

    (** [blocks_to_be_produced dsl n] uses [dsl]'s wait condition to wait for [n] blocks *)
    val blocks_to_be_produced :
      Inputs.Dsl.t -> int -> unit Integration_test_lib.Malleable_error.t

    (** [nodes_to_synchronize dsl nodes] uses [dsl]'s wait condition to wait for [nodes] to synchronize with each other *)
    val nodes_to_synchronize :
         Inputs.Dsl.t
      -> Inputs.Engine.Network.Node.t list
      -> unit Integration_test_lib.Malleable_error.t

    (** TODO: [ledger_proofs_emitted_since_genesis dsl ~test_config ~num_proofs]  *)
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

    (** [signed_command_to_be_included_in_frontier dsl ~txn_hash ~node_included_in]
        uses [dsl]'s wait condition to wait for the signed command with transaction
        hash [txn_hash] to be included in [node_included_in] *)
    val signed_command_to_be_included_in_frontier :
         Inputs.Dsl.t
      -> txn_hash:Mina_transaction.Transaction_hash.t
      -> node_included_in:[ `Any_node | `Node of Inputs.Engine.Network.Node.t ]
      -> unit Integration_test_lib.Malleable_error.t

    (** [with_timeouts dsl ~condition ~soft_timeout ~hard_timeout] uses [dsl]'s wait
        condition to wait for [condition] with a [soft_timeout] and a [hard_timeout] *)
    val with_timeouts :
         Inputs.Dsl.t
      -> condition:Inputs.Dsl.Wait_condition.t
      -> soft_timeout:Integration_test_lib.Network_time_span.t
      -> hard_timeout:Integration_test_lib.Network_time_span.t
      -> unit Integration_test_lib.Malleable_error.t

    (** [zkapp_to_be_included_in_frontier dsl ~has_failures ~zkapp_command ~soft_slots]
        uses [dsl]'s wait condition to wait for [zkapp_command] to be included in the
        transition frontier with a soft timeout equal to the span of [soft_slots] slots
        and a hard timeout equal to the span of [2 * soft_slots] slots *)
    val zkapp_to_be_included_in_frontier :
         Inputs.Dsl.t
      -> has_failures:bool
      -> zkapp_command:Mina_base.Zkapp_command.t
      -> soft_slots:int
      -> unit Integration_test_lib.Malleable_error.t
  end

  (** [all_nodes network] returns the list of all nodes in the [network] *)
  val all_nodes : Inputs.Engine.Network.t -> Inputs.Engine.Network.Node.t list

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

  (** TODO: [check_common_prefixes ~tolerance ~logger chains]*)
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

  (** TODO: [assert_peers_completely_connected nodes_and_responses]*)
  val assert_peers_completely_connected :
       (Inputs.Engine.Network.Node.t * (string * string list)) list
    -> unit Integration_test_lib.Malleable_error.t

  (** TODO: [assert_peers_cant_be_partitioned ~max_disconnection nodes_and_responses]*)
  val assert_peers_cant_be_partitioned :
       max_disconnections:int
    -> (_ * (string * string list)) list
    -> ( unit Integration_test_lib.Malleable_error.Result_accumulator.t
       , Integration_test_lib.Malleable_error.Hard_fail.t )
       result
       Async.Deferred.t

  module Zkapp_util : sig
    (** [send_batch ~logger node zkapp_commands] returns the result of sending
        all commands in [zkapp_commands] *)
    val send_batch :
         logger:Logger.t
      -> Inputs.Engine.Network.Node.t
      -> Mina_base.Zkapp_command.t list
      -> ( unit Integration_test_lib.Malleable_error.Result_accumulator.t
         , Integration_test_lib.Malleable_error.Hard_fail.t )
         result
         Async.Deferred.t

    (** [send ~logger node zkapp_command] returns the result of [node]
        sending the [zkapp_command] to the network, logs with [logger] *)
    val send :
         logger:Logger.t
      -> Inputs.Engine.Network.Node.t
      -> Mina_base.Zkapp_command.t
      -> ( unit Integration_test_lib.Malleable_error.Result_accumulator.t
         , Integration_test_lib.Malleable_error.Hard_fail.t )
         result
         Async.Deferred.t

    (** [send_invalid ~logger node zkapp_command substring_of_error_msg]
        returns the result of [node] sending an invalid [zkapp_command]
        expecting error [substring_of_error_msg], logs with [logger] *)
    val send_invalid :
         logger:Logger.t
      -> Inputs.Engine.Network.Node.t
      -> Mina_base.Zkapp_command.t
      -> string
      -> ( unit Integration_test_lib.Malleable_error.Result_accumulator.t
         , Integration_test_lib.Malleable_error.Hard_fail.t )
         result
         Async.Deferred.t

    (** TODO: [get_pooled_zkapp_commands ~logger node ~pk] *)
    val get_pooled_zkapp_commands :
         logger:Logger.t
      -> Inputs.Engine.Network.Node.t
      -> pk:Signature_lib.Public_key.Compressed.t
      -> ( string list Integration_test_lib.Malleable_error.Result_accumulator.t
         , Integration_test_lib.Malleable_error.Hard_fail.t )
         result
         Async.Deferred.t

    (** [compatible_item item1 item2 ~equal] compares items of different types
        [item1] and [item2] for equality under [equal] *)
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

  module Archive_node : sig
    (** [run_and_check_replayer ~logger network] runs and checks the archive node's replayer *)
    val run_and_check_replayer :
         logger:Logger.t
      -> Inputs.Engine.Network.t
      -> ( unit Integration_test_lib.Malleable_error.Result_accumulator.t
         , Integration_test_lib.Malleable_error.Hard_fail.t )
         result
         Async.Deferred.t
  end
end
