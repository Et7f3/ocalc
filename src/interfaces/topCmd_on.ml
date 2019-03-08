open Utils
open Lexer
open Type
open Parser
open Moteur

let detect_print e =
  if true then
    print_endline (texte_de_expr (eval (expr_de_texte e)))
  else
  if est_entier10 e then
    Printf.printf "int: %s\n" e
  else if est_variable e then
    Printf.printf "variable: %s\n" e
  else
    print_endline e

let main (argc, argv) =
  let () = print_endline "hello world" in
  let () = print_endline "type exit to exit" in
  let rec l () =
    let () = print_string "# " in
    let ligne = read_line () in
    if ligne <> "exit" && ligne <> "quit" then
      let () =
        if parenthese_correcte ligne then
          if false && contient_texte ligne ['+'; '-'] then
            let rec boucle = function
                [] -> ()
              | e :: l ->
                let () = detect_print e in
                boucle l
            in boucle (couper_texte ligne ['+'; '-'])
          else
            detect_print ligne
        else
          print_endline "vérifiez vos parenthèses !!!"
      in l () in
  l ()
