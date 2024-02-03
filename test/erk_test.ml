let () =
  let open Erk in
  let test_tree = Node ("a", Empty, Node ("b", Empty, Empty)) in
  assert (
    string_of_tree (fun x -> x) test_tree = "Node (a, Empty, Node (b, Empty, Empty))")
;;
