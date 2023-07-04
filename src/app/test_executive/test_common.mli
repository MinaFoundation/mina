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
    (** [node_to_initialize dsl node] uses [dsl]'s wait condition to wait for [node] to initialize *)
    val node_to_initialize :
         Inputs.Dsl.t
      -> Inputs.Engine.Network.Node.t
      -> unit Integration_test_lib.Malleable_error.t

    (** [all_nodes_to_initialize network dsl] uses [dsl]'s wait condition to wait for all nodes in the [network] to initialize *)
    val all_nodes_to_initialize :
         Inputs.Engine.Network.t
      -> Inputs.Dsl.t
      -> unit Integration_test_lib.Malleable_error.t

    (** [nodes_to_synchronize dsl nodes] uses [dsl]'s wait condition to wait for [nodes] to synchronize with each other *)
    val nodes_to_synchronize :
         Inputs.Dsl.t
      -> Inputs.Engine.Network.Node.t list
      -> unit Integration_test_lib.Malleable_error.t

    (** [blocks_to_be_produced dsl n] uses [dsl]'s wait condition to wait for [n] blocks *)
    val blocks_to_be_produced :
      Inputs.Dsl.t -> int -> unit Integration_test_lib.Malleable_error.t

    (** TODO: [payments_to_be_included_in_transition_frontier ~logger ~dsl ~hashlist n] *)
    val payments_to_be_included_in_transition_frontier :
         logger:Logger.t
      -> dsl:Inputs.Dsl.t
      -> hashlist:Mina_transaction.Transaction_hash.t list
      -> int
      -> unit Integration_test_lib.Malleable_error.t

    (** TODO: [with_timeouts dsl ~condition ~soft_timeout ~hard_timeout] *)
    val with_timeouts :
         Inputs.Dsl.t
      -> condition:Inputs.Dsl.Wait_condition.t
      -> soft_timeout:Integration_test_lib.Network_time_span.t
      -> hard_timeout:Integration_test_lib.Network_time_span.t
      -> unit Integration_test_lib.Malleable_error.t
  end

  (** [get_node network name] returns the node in the [network] with the given [name] *)
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

  (** TODO: [check_common_prefixes]*)
  val check_common_prefixes :
       tolerance:int
    -> logger:Logger.t
    -> string list list
    -> unit Integration_test_lib.Malleable_error.t

  (** TODO: [fetch_connectivity_data]*)
  val fetch_connectivity_data :
       logger:Logger.t
    -> Inputs.Engine.Network.Node.t list
    -> (Inputs.Engine.Network.Node.t * (string * string list)) list
       Integration_test_lib.Malleable_error.t

  (** TODO: [assert_peers_completely_connected]*)
  val assert_peers_completely_connected :
       (Inputs.Engine.Network.Node.t * (string * string list)) list
    -> unit Integration_test_lib.Malleable_error.t

  (** TODO: [assert_peers_cant_be_partitioned]*)
  val assert_peers_cant_be_partitioned :
       max_disconnections:int
    -> ('a * (string * string list)) list
    -> ( unit Integration_test_lib.Malleable_error.Result_accumulator.t
       , Integration_test_lib.Malleable_error.Hard_fail.t )
       result
       Async.Deferred.t

  (** TODO: [send_zkapp_batch]*)
  val send_zkapp_batch :
       logger:Logger.t
    -> Inputs.Engine.Network.Node.t
    -> Mina_base.Zkapp_command.t list
    -> ( unit Integration_test_lib.Malleable_error.Result_accumulator.t
       , Integration_test_lib.Malleable_error.Hard_fail.t )
       result
       Async.Deferred.t

  (** TODO: [send_zkapp]*)
  val send_zkapp :
       logger:Logger.t
    -> Inputs.Engine.Network.Node.t
    -> Mina_base.Zkapp_command.t
    -> ( unit Integration_test_lib.Malleable_error.Result_accumulator.t
       , Integration_test_lib.Malleable_error.Hard_fail.t )
       result
       Async.Deferred.t

  (** TODO: [send_invalid_zkapp]*)
  val send_invalid_zkapp :
       logger:Logger.t
    -> Inputs.Engine.Network.Node.t
    -> Mina_base.Zkapp_command.t
    -> string
    -> ( unit Integration_test_lib.Malleable_error.Result_accumulator.t
       , Integration_test_lib.Malleable_error.Hard_fail.t )
       result
       Async.Deferred.t

  (** TODO: [send_invalid_payment]*)
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

  (** TODO: [get_account_permissions]*)
  val get_account_permissions :
       logger:Logger.t
    -> Inputs.Engine.Network.Node.t
    -> Mina_base.Account_id.t
    -> ( Mina_base.Permissions.t
         Integration_test_lib.Malleable_error.Result_accumulator.t
       , Integration_test_lib.Malleable_error.Hard_fail.t )
       result
       Async.Deferred.t

  (** TODO: [get_account_update]*)
  val get_account_update :
       logger:Logger.t
    -> Inputs.Engine.Network.Node.t
    -> Mina_base.Account_id.t
    -> ( Mina_base.Account_update.Update.t
         Integration_test_lib.Malleable_error.Result_accumulator.t
       , Integration_test_lib.Malleable_error.Hard_fail.t )
       result
       Async.Deferred.t

  (* val get_pooled_zkapp_commands :
     logger:Logger.t ->
     Inputs.Engine.Network.Node.t ->
     pk:Signature_lib.Public_key.Compressed.t ->
     (string list
      Integration_test_lib.Malleable_error.Result_accumulator.t,
      Integration_test_lib.Malleable_error.Hard_fail.t)
     result Async.Deferred.t *)
  (** TODO: [get_pooled_zkapp_commands]*)

  (** TODO: [compatible_item]*)
  val compatible_item :
       'a Mina_base.Zkapp_basic.Set_or_keep.t
    -> 'b Mina_base.Zkapp_basic.Set_or_keep.t
    -> equal:('a -> 'b -> bool)
    -> bool

  (** TODO: [compatible_updates]*)
  val compatible_updates :
       ledger_update:Mina_base.Account_update.Update.t
    -> requested_update:Mina_base.Account_update.Update.t
    -> bool

  (** TODO: [check_replayer_logs]*)
  val check_replayer_logs :
       logger:Logger.t
    -> string
    -> ( unit Integration_test_lib.Malleable_error.Result_accumulator.t
       , Integration_test_lib.Malleable_error.Hard_fail.t )
       result
       Async.Deferred.t
end
