open Core
open Currency
open Mina_base
open Mina_transaction
open Unsigned

module T = struct
  type t = { nonce_gap : UInt32.t; fee : Fee.t; weight : int } [@@deriving eq]

  (* Nonce gap is always more important than fee or fee_per_wu,
     because a transaction with *any* nonce gap cannot ever be
     applied. Therefore, it should be considered for drop first.
     Comparison by nonce gap is inverted, because high nonce gap
     *decreases* transaction's value rather than increase it. *)
  let compare t1 t2 =
    let by_gap = -UInt32.(compare t1.nonce_gap t2.nonce_gap) in
    if by_gap <> 0 then by_gap else Fee.(compare t1.fee t2.fee)

  let sexp_of_t { nonce_gap; fee; weight } =
    Sexp.(
      List
        [ List [ Atom "nonce_gap"; Atom (UInt32.to_string nonce_gap) ]
        ; List [ Atom "fee"; Fee.sexp_of_t fee ]
        ; List [ Atom "weight"; sexp_of_int weight ]
        ])

  let fields = String.Set.of_list [ "nonce_gap"; "fee"; "weight" ]

  let read_field acc = function
    | Sexp.List [ Atom name; Atom value ] ->
        if String.Set.mem fields name then
          String.Map.add_exn acc ~key:name ~data:value
        else failwithf "Invalid field: %s" name ()
    | s ->
        failwithf "Invalid field: %s" (Sexp.to_string s) ()

  let t_of_sexp = function
    | Sexp.Atom s ->
        failwithf "Expected list, got atom: %s" s ()
    | List fs ->
        let data = List.fold fs ~init:String.Map.empty ~f:read_field in
        { nonce_gap = UInt32.of_string @@ String.Map.find_exn data "nonce_gap"
        ; fee = Fee.of_string @@ String.Map.find_exn data "fee"
        ; weight = Int.of_string @@ String.Map.find_exn data "weight"
        }
end

include T

(* Computing nonce gap requires access to an account's queue, so
   it must be found separately. *)
let of_transaction ~nonce_gap
    (t : Transaction_hash.User_command_with_valid_signature.t) =
  let unchecked =
    Transaction_hash.User_command_with_valid_signature.command t
  in
  { fee = User_command.fee unchecked
  ; weight = User_command.weight unchecked
  ; nonce_gap
  }

let is_applicable { nonce_gap; _ } = UInt32.(equal nonce_gap zero)

module Map = Map.Make (T)
