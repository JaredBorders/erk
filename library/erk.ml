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

(* List of leaves must be non-empty and *)
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

let%test "Testing validate_list_length..." =
  validate_list_length [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ]
  && (not @@ validate_list_length [ "a"; "b"; "c"; "d"; "e"; "f" ])
  && (not @@ validate_list_length [ "a"; "b"; "c" ])
  && (not @@ validate_list_length [ "a" ])
  && (not @@ validate_list_length [])
;;

let build_parents leaves =
  let rec build_aux leaves acc =
    match leaves with
    | [] -> List.rev acc
    | x :: y :: rest -> build_aux rest (concat_hash x y :: acc)
    | _ -> failwith "build_parents: odd number of leaves"
  in
  build_aux leaves []
;;

let%test "Testing build_parents..." =
  [%compare.equal: string list]
    (build_parents [ "a"; "b"; "c"; "d" ])
    [ hash_string @@ "a" ^ "b"; hash_string @@ "c" ^ "d" ]
;;