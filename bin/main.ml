let () =
  let open Erk in
  print_endline "********************\n\n🌳🌳🌳\n";
  let tree = Node (1, Node (2, Empty, Empty), Node (3, Empty, Empty)) in
  print_endline (string_of_tree string_of_int tree);
  print_endline "\n********************\n"
;;
