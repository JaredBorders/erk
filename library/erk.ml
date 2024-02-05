open! Base

type merkle_tree =
  | Empty
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

let get_hashes leaves hash fill =
  let rec f' acc = function
    | [] -> List.rev acc
    | x :: y :: rest -> f' (hash x y :: acc) rest
    | x :: _ -> List.rev (hash x fill :: acc)
  in
  let rec f'' (acc : 'a list) = function
    | [] | [ _ ] -> acc
    | lst ->
      let result = f' [] lst in
      f'' (result @ acc) result
  in
  (if List.length leaves = 1
   then f'' [ hash (List.hd_exn leaves) fill ] leaves
   else f'' [] leaves)
  @ leaves
;;

let print_hashes lst = List.iter lst ~f:(fun x -> Stdio.print_endline x)

let%test "Testing get_hashes..." =
  [%compare.equal: string list] (get_hashes [] concat_hash "") []
  && [%compare.equal: string list]
       (get_hashes [ "a" ] concat_hash "")
       [ concat_hash "a" ""; "a" ]
  && [%compare.equal: string list]
       (get_hashes [ "a"; "b" ] concat_hash "")
       [ concat_hash "a" "b"; "a"; "b" ]
  && [%compare.equal: string list]
       (get_hashes [ "a"; "b"; "c" ] concat_hash "")
       [ concat_hash (concat_hash "a" "b") (concat_hash "c" "")
       ; concat_hash "a" "b"
       ; concat_hash "c" ""
       ; "a"
       ; "b"
       ; "c"
       ]
  && [%compare.equal: string list]
       (get_hashes [ "a"; "b"; "c"; "d" ] concat_hash "")
       [ concat_hash (concat_hash "a" "b") (concat_hash "c" "d")
       ; concat_hash "a" "b"
       ; concat_hash "c" "d"
       ; "a"
       ; "b"
       ; "c"
       ; "d"
       ]
  && [%compare.equal: string list]
       (get_hashes [ "a"; "b"; "c"; "d"; "e"; "f" ] (fun x y -> x ^ y) "?")
       [ "abcdef?"; "abcd"; "ef?"; "ab"; "cd"; "ef"; "a"; "b"; "c"; "d"; "e"; "f" ]
  && [%compare.equal: int list]
       (get_hashes [ 1; 2; 3; 4 ] (fun x y -> x * y * 10) 0)
       [ 24000; 20; 120; 1; 2; 3; 4 ]
;;

(* let merkle_tree_from_list leaves =
   match validate_list_length leaves with
   | false -> failwith "merkle_tree_from_list: invalid list length"
   | true -> Empty
   ;;

   let example_array_of_leaves = [ "a"; "b"; "c"; "d" ]

   let example_merkle_tree =
   let leaf1 = List.nth_exn example_array_of_leaves 0 in
   let leaf2 = List.nth_exn example_array_of_leaves 1 in
   let leaf3 = List.nth_exn example_array_of_leaves 2 in
   let leaf4 = List.nth_exn example_array_of_leaves 3 in
   let digest_1 = concat_hash leaf1 leaf2 in
   let digest_2 = concat_hash leaf3 leaf4 in
   let root = concat_hash digest_1 digest_2 in
   Node
   ( root
   , Node (digest_1, Node (leaf1, Empty, Empty), Node (leaf2, Empty, Empty))
   , Node (digest_2, Node (leaf3, Empty, Empty), Node (leaf4, Empty, Empty)) )
   ;;

   let%test "Testing merkle_tree_from_list..." =
   [%compare.equal: merkle_tree]
   example_merkle_tree
   (merkle_tree_from_list example_array_of_leaves)
   ;; *)
