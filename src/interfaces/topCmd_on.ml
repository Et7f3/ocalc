open Utils
open Lexer
open Type

let main (argc, argv) =
  let () = print_endline "hello world" in
  let () = print_endline "type exit to exit" in
  let rec l () =
    let () = print_string "# " in
    let ligne = read_line () in
    if ligne <> "exit" && ligne <> "quit" then
      let () =
        if parenthese_correcte ligne then
          if contient_texte ligne ['+'; '-'] then
          let rec boucle = function
              [] -> ()
            | e :: l ->
              let () =
                if est_entier10 e then
                  Printf.printf "int: %s\n" e
                else
                  print_endline e
              in boucle l
          in boucle (couper_texte ligne ['+'; '-'])
        else if est_entier10 ligne then
            Printf.printf "int: %s\n" ligne
          else
            print_endline ligne
        else
          print_endline "vérifiez vos parenthèses !!!"
      in l () in
  l ()
