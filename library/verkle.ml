open! Base

type verkle_tree =
  | Empty
  | Node of string * verkle_tree * verkle_tree
[@@deriving compare]
