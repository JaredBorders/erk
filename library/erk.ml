type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

let rec string_of_tree to_string = function
  | Empty -> "Empty"
  | Node (value, left, right) ->
    "Node ("
    ^ to_string value
    ^ ", "
    ^ string_of_tree to_string left
    ^ ", "
    ^ string_of_tree to_string right
    ^ ")"
;;
