open Utils

let main (argc, argv) =
  let () = print_endline "hello world" in
  let () = print_endline "type exit to exit" in
  let rec l () =
    let () = print_string "# " in
    let ligne = read_line () in
    if ligne <> "exit" && ligne <> "quit" then
      let () =
        contient_texte ligne ['+'] |> print_bool
      in l () in
  l ()
