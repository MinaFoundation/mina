(** Utilities for dealing with "multimaps" or mapsets. *)

open Core

(** A map from 'ks to sets of 'vs, using the provided comparators. *)
type ('k, 'v, 'cmpS, 'cmpM) t = ('k, ('v, 'cmpS) Set.t, 'cmpM) Map.t

(** Remove an element from a mapset or do nothing if the key is absent. *)
let remove : ('k, 'v, 'cmpS, 'cmpM) t -> 'k -> 'v -> ('k, 'v, 'cmpS, 'cmpM) t =
 fun map k v ->
  Map.change map k ~f:(fun set ->
      let open Option.Let_syntax in
      let%bind s = set in
      let set' = Set.remove s v in
      Option.some_if (not (Set.is_empty set')) set' )

(** Remove an element from a mapset, throwing if the key is absent. *)
let remove_exn :
    ('k, 'v, 'cmpS, 'cmpM) t -> 'k -> 'v -> ('k, 'v, 'cmpS, 'cmpM) t =
 fun map k v ->
  let newset = Map.find_exn map k |> Fn.flip Set.remove v in
  if Set.is_empty newset then Map.remove map k
  else Map.set map ~key:k ~data:newset

(* Add an element to a mapset. *)
let insert :
       ('v, 'cmpS) Set.comparator
    -> ('k, 'v, 'cmpS, 'cmpM) t
    -> 'k
    -> 'v
    -> ('k, 'v, 'cmpS, 'cmpM) t =
 fun comparator map k v ->
  Map.change map k ~f:(fun set_opt ->
      match set_opt with
      | None ->
          Some (Set.singleton comparator v)
      | Some set ->
          Some (Set.add set v) )
