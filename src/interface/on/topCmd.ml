(*open Parser*)

let main (argc, argv) =
  let () = print_endline "hello world" in
  let rec l () =
    let ligne = read_line () in
    if ligne <> "exit" then
      l () in
  l ()
