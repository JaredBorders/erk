open! Base

type merkle_tree =
  | Leaf of string
  | Node of string * merkle_tree * merkle_tree
[@@deriving compare]

let hash_string s = Digestif.SHA256.(to_hex (digest_string s))

let%test "Testing hashing a string..." =
  String.equal
    (hash_string "potato")
    "e91c254ad58860a02c788dfb5c1a65d6a8846ab1dc649631c7db16fef4af2dec"
;;

let concat_hash a b = hash_string (a ^ b)

let%test "Testing concatenating and hashing two strings..." =
  String.equal
    (concat_hash "potato" "tomato")
    "7735464dca434e1d3d7198ccb7a99daf32f0ba13e1182de2eab8daff487b061b"
;;

let validate_list_length lst =
  let len = List.length lst in
  if len = 0 || len = 1
  then false
  else (
    let rec f acc =
      match acc with
      | 1 -> true
      | x -> if x % 2 = 0 then f (x / 2) else false
    in
    f len)
;;

let%test "Testing validating merkle tree leaves length..." =
  validate_list_length [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ]
  && (not @@ validate_list_length [ "a"; "b"; "c"; "d"; "e"; "f" ])
  && (not @@ validate_list_length [ "a"; "b"; "c" ])
  && (not @@ validate_list_length [ "a" ])
  && (not @@ validate_list_length [])
;;

let build_parents leaves =
  let rec build_aux leaves acc =
    match leaves with
    | [] -> acc
    | x :: y :: rest -> build_aux rest (concat_hash x y :: acc)
    | _ -> failwith "build_parents: odd number of leaves"
  in
  build_aux leaves [] |> List.rev
;;

let%test "Testing building parents list..." =
  [%compare.equal: string list]
    (build_parents [ "a"; "b"; "c"; "d" ])
    [ concat_hash "a" "b"; concat_hash "c" "d" ]
  && [%compare.equal: string list] (build_parents [ "a"; "b" ]) [ concat_hash "a" "b" ]
;;

let aggregate_parents (leaves : string list) : string list list =
  let rec aggregate_parents_aux (leaves : string list) (acc : string list list) =
    match leaves with
    | [] | [ _ ] -> acc
    | non_empty_list ->
      let parent_list = build_parents non_empty_list in
      aggregate_parents_aux parent_list (parent_list :: acc)
  in
  aggregate_parents_aux leaves [ leaves ]
;;

let%test "Testing aggregating parent lists..." =
  [%compare.equal: string list list]
    (aggregate_parents [ "a"; "b"; "c"; "d" ])
    [ [ concat_hash (concat_hash "a" "b") (concat_hash "c" "d") ]
    ; [ concat_hash "a" "b"; concat_hash "c" "d" ]
    ; [ "a"; "b"; "c"; "d" ]
    ]
;;