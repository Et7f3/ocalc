open Lien

let () =
  let argv = Sys.argv in
  let argc = Array.length argv in
  let toplevel_handlers = ref [] in
  let toplevels = [
    TopCmd.main; (* read evale print loop *)
    (* TopFile.main; (* read all files as arguments and procede file by file *)
       TopGui.main; (* show a beautiful graphical user interface *)
       TopServeur.main (* launch a serveur that liste on specific port passed *)
    *)
  ]
  in let rec l = function
        [] -> List.iter Thread.join (!toplevel_handlers)
      |  e :: liste ->
        let () = print_endline "on a un lancé un processus léger" in
        let () = toplevel_handlers := (Thread.create e (argc, argv)) :: !toplevel_handlers in
        l liste
  in l toplevels
