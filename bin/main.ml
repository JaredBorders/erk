let () =
  let open Erk in
  print_endline "********************\n\n🌳🌳\n";
  Stdio.print_string @@ Merkle.hash_string "potato";
  print_endline "\n********************\n"
;;
