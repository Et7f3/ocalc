open Type
open Utils
open Lexer
open Parser

let (compteur_de_test, test_valide, numero_de_test, sous_test, sous_test_valide) = ref 0, ref 0, ref 0, ref 0, ref 0

(*numéro de *)
let (test_total, test_valide_total, suite, test, test_valide) = ref 0, ref 0, ref 0, ref 0, ref 0

let rec test_unitaire_assert a b =
  let () =
    if a <> b then
      Printf.printf "Le test #%d n'as pas produit la valeure attendue\n" !test
    else
      let () = test_valide := !test_valide + 1 in
      test_valide_total := !test_valide_total + 1
  in let () = test_total := !test_total + 1 in
  test := !test + 1

let init_suite name =
  let () = Printf.printf "\n\n\x1b[4mSuite #%d\x1b[0m: %s\n" !suite name in
  suite := !suite + 1

let fin_suite () =
  let () =
    Printf.printf "%3.2f %% de tests passés pour cette suite\n" (100. *. float_of_int !test_valide /. float_of_int !test) in
  let () = test_valide := 0 in
  test := 0

let reinit_test_suite () =
  let () = test_total := 0 in
  let () = test_valide_total := 0 in
  let () = suite := 0 in
  let () = test := 0 in
  test_valide := 0


let () = init_suite "est_entier"

let () = test_unitaire_assert (est_entier "123" 10) true
let () = test_unitaire_assert (est_entier "-123" 10) true
let () = test_unitaire_assert (est_entier "+123" 10) true
let () = test_unitaire_assert (est_entier "007" 10) true
let () = test_unitaire_assert (est_entier "0" 10) true
let () = test_unitaire_assert (est_entier "" 10) false
let () = test_unitaire_assert (est_entier "123a" 10) false
let () = test_unitaire_assert (est_entier "_123" 10) false
let () = test_unitaire_assert (est_entier "1u23" 10) false

let () = fin_suite ()

let () = init_suite "est_entier_base"

let () = test_unitaire_assert (est_entier_base "12310") false
let () = test_unitaire_assert (est_entier_base "123_10") true
let () = test_unitaire_assert (est_entier_base "(123)_10") true
let () = test_unitaire_assert (est_entier_base "123)_10") false
let () = test_unitaire_assert (est_entier_base "(123_10") false
let () = test_unitaire_assert (est_entier_base "_123_10") false
let () = test_unitaire_assert (est_entier_base "g123_10") false
let () = test_unitaire_assert (est_entier_base "123u_10") false
let () = test_unitaire_assert (est_entier_base "12b3_10") false
let () = test_unitaire_assert (est_entier_base "à12b3_10") false

let () = fin_suite ()

let () = init_suite "est_variable"

let () = test_unitaire_assert (est_variable "a") true
let () = test_unitaire_assert (est_variable "x") true
let () = test_unitaire_assert (est_variable "x'") true
let () = test_unitaire_assert (est_variable "pi") true
let () = test_unitaire_assert (est_variable "pi2") true
let () = test_unitaire_assert (est_variable "pi-2") false
let () = test_unitaire_assert (est_variable "pi_t") true
let () = test_unitaire_assert (est_variable "pi_2") true (*Va être confondu avec est_entier_base ?*)
let () = test_unitaire_assert (est_variable "a_") false (*Termine par un _ *)
let () = test_unitaire_assert (est_variable "Ax") false (*Constante qui commence par une maj ? Matrice ?*)
let () = test_unitaire_assert (est_variable "x'''''") true (*apostrophe ok*)
let () = test_unitaire_assert (est_variable "x'''''123") true
let () = test_unitaire_assert (est_variable "xZE") true (*Majuscule pas à la première ok*)

let () = fin_suite ()

let () = init_suite "contient_texte"

let () = test_unitaire_assert (contient_texte "123" ["+"]) false
let () = test_unitaire_assert (contient_texte "123+456" ["+"]) true
let () = test_unitaire_assert (contient_texte "123+456" ["-"]) false
let () = test_unitaire_assert (contient_texte "123-456+789" ["-"; "+"]) true

let () = fin_suite ()

let () = init_suite "expr_de_texte"

let parse = []
let () = test_unitaire_assert (expr_de_texte parse "3445") (Textenonvalide  "3445")
let () = test_unitaire_assert (expr_de_texte parse "x") (Textenonvalide  "x")
let parse = (est_variable, variable_de_texte) :: parse;;
let parse = ((fun t -> est_entier t 10), grandentier_de_texte) :: parse;;
let () = test_unitaire_assert (expr_de_texte parse "3445") (Entier (1, [5; 4; 4; 3]))
let () = test_unitaire_assert (expr_de_texte parse "-3445") (Entier (-1, [5; 4; 4; 3]))
let () = test_unitaire_assert (expr_de_texte parse "x") (Variable ("x"))

let () = fin_suite ()

let () = Printf.printf "\n\n%3.2f %% des tests global passés\n" (100. *. float_of_int !test_valide_total /. float_of_int !test_total)
let () = reinit_test_suite ()
