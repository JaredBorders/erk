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
  let rec f acc = function
    | [] -> List.rev acc
    | x :: y :: rest -> f (hash x y :: acc) rest
    | x :: _ -> List.rev (hash x fill :: acc)
  in
  let rec f' (acc : 'a list) = function
    | [] | [ _ ] -> acc
    | lst ->
      let result = f [] lst in
      f' (result @ acc) result
  in
  (if List.length leaves = 1
   then f' [ hash (List.hd_exn leaves) fill ] leaves
   else f' [] leaves)
  @ leaves
;;

let print_hashes lst = List.iter lst ~f:(fun x -> Stdio.print_endline x)

let%test "Testing get hashes from empty list of leaves..." =
  [%compare.equal: string list] (get_hashes [] concat_hash "") []
;;

let%test "Testing get hashes from list of single leaf..." =
  [%compare.equal: string list]
    (get_hashes [ "a" ] concat_hash "")
    [ concat_hash "a" ""; "a" ]
;;

let%test "Testing get hashes from list of two leaves..." =
  [%compare.equal: string list]
    (get_hashes [ "a"; "b" ] concat_hash "")
    [ concat_hash "a" "b"; "a"; "b" ]
;;

let%test "Testing get hashes from list of three leaves..." =
  [%compare.equal: string list]
    (get_hashes [ "a"; "b"; "c" ] concat_hash "")
    [ concat_hash (concat_hash "a" "b") (concat_hash "c" "")
    ; concat_hash "a" "b"
    ; concat_hash "c" ""
    ; "a"
    ; "b"
    ; "c"
    ]
;;

let%test "Testing get hashes from list of four leaves..." =
  [%compare.equal: string list]
    (get_hashes [ "a"; "b"; "c"; "d" ] concat_hash "")
    [ concat_hash (concat_hash "a" "b") (concat_hash "c" "d")
    ; concat_hash "a" "b"
    ; concat_hash "c" "d"
    ; "a"
    ; "b"
    ; "c"
    ; "d"
    ]
;;

let%test "Testing get_hashes with list containing six elements and custom hash \
          function..."
  =
  [%compare.equal: string list]
    (get_hashes [ "a"; "b"; "c"; "d"; "e"; "f" ] (fun x y -> x ^ y) "?")
    [ "abcdef?"; "abcd"; "ef?"; "ab"; "cd"; "ef"; "a"; "b"; "c"; "d"; "e"; "f" ]
;;

let%test "Testing get_hashes with list of integers and custom hash function..." =
  [%compare.equal: int list]
    (get_hashes [ 1; 2; 3; 4 ] (fun x y -> x * y * 10) 0)
    [ 24000; 20; 120; 1; 2; 3; 4 ]
;;

let get_children idx = (idx * 2) + 1, (idx * 2) + 2

let%test "Testing getting children indices..." =
  [%compare.equal: int * int] (get_children 1) (3, 4)
  && [%compare.equal: int * int] (get_children 3) (7, 8)
;;

let%test "Testing getting children elements from indices..." =
  let test_list =
    [ "abcdefgh"
    ; "abcd"
    ; "efgh"
    ; "ab"
    ; "cd"
    ; "ef"
    ; "gh"
    ; "a"
    ; "b"
    ; "c"
    ; "d"
    ; "e"
    ; "f"
    ; "g"
    ; "h"
    ]
  in
  [%compare.equal: string * string]
    ( List.nth_exn test_list (fst (get_children 3))
    , List.nth_exn test_list (snd (get_children 3)) )
    ("a", "b")
;;

let create hash find fill leaves =
  if List.is_empty leaves
  then Empty
  else (
    let hashes = get_hashes leaves hash fill in
    let rec build_tree current cont =
      match find current with
      | a, b when b < List.length hashes && a < List.length hashes ->
        build_tree a (fun aTree ->
          build_tree b (fun bTree ->
            cont (Node (List.nth_exn hashes current, aTree, bTree))))
      | a, _ when a < List.length hashes ->
        build_tree a (fun aTree ->
          cont (Node (List.nth_exn hashes current, aTree, Empty)))
      | _ -> cont (Node (List.nth_exn hashes current, Empty, Empty))
    in
    build_tree 0 (fun t -> t))
;;

let%test "Testing create with empty list..." =
  let empty_list = [] in
  let hash x y = x ^ y in
  [%compare.equal: merkle_tree] (create hash get_children "?" empty_list) Empty
;;

let%test "Testing create with list containing one element..." =
  let test_list1 = [ "a" ] in
  let hash x y = x ^ y in
  [%compare.equal: merkle_tree]
    (create hash get_children "?" test_list1)
    (Node ("a?", Node ("a", Empty, Empty), Empty))
;;

let%test "Testing create with list containing two elements..." =
  let test_list2 = [ "a"; "b" ] in
  let hash x y = x ^ y in
  [%compare.equal: merkle_tree]
    (create hash get_children "?" test_list2)
    (Node ("ab", Node ("a", Empty, Empty), Node ("b", Empty, Empty)))
;;

let%test "Testing create with list containing three elements..." =
  let test_list3 = [ "a"; "b"; "c" ] in
  let hash x y = x ^ y in
  [%compare.equal: merkle_tree]
    (create hash get_children "?" test_list3)
    (Node
       ( "abc?"
       , Node ("ab", Node ("a", Empty, Empty), Node ("b", Empty, Empty))
       , Node ("c?", Node ("c", Empty, Empty), Empty) ))
;;

let%test "Testing create with list containing four elements..." =
  let test_list4 = [ "a"; "b"; "c"; "d" ] in
  let hash x y = x ^ y in
  [%compare.equal: merkle_tree]
    (create hash get_children "?" test_list4)
    (Node
       ( "abcd"
       , Node ("ab", Node ("a", Empty, Empty), Node ("b", Empty, Empty))
       , Node ("cd", Node ("c", Empty, Empty), Node ("d", Empty, Empty)) ))
;;

let%test "Testing create with list containing eight elements..." =
  let test_list5 = [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] in
  let hash x y = x ^ y in
  [%compare.equal: merkle_tree]
    (create hash get_children "?" test_list5)
    (Node
       ( "abcdefgh"
       , Node
           ( "abcd"
           , Node ("ab", Node ("a", Empty, Empty), Node ("b", Empty, Empty))
           , Node ("cd", Node ("c", Empty, Empty), Node ("d", Empty, Empty)) )
       , Node
           ( "efgh"
           , Node ("ef", Node ("e", Empty, Empty), Node ("f", Empty, Empty))
           , Node ("gh", Node ("g", Empty, Empty), Node ("h", Empty, Empty)) ) ))
;;
