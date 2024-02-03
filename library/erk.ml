(* Define a type for Merkle tree nodes *)
type merkle_tree =
  | Leaf of string
  | Node of string * merkle_tree * merkle_tree

(* Function to compute SHA256 hash of a string *)
let hash_string s = Digestif.SHA256.(to_hex (digest_string s))

let%test "Testing hashing a string..." =
  String.equal
    (hash_string "potato")
    "e91c254ad58860a02c788dfb5c1a65d6a8846ab1dc649631c7db16fef4af2dec"
;;
