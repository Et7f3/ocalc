open Lexer

let (compteur_de_test, test_valide) = ref 0, ref 0

let rec test_unitaire_assert a b =
  let () =
    if a <> b then
      Printf.printf "Le test #%d n'as pas produit la valeure attendue\n" !compteur_de_test
    else
      test_valide := !test_valide + 1
in compteur_de_test := !compteur_de_test + 1

let () = test_unitaire_assert (est_entier "123" 10) true
let () = test_unitaire_assert (est_entier "-123" 10) true
let () = test_unitaire_assert (est_entier "+123" 10) true
let () = test_unitaire_assert (est_entier "007" 10) true
let () = test_unitaire_assert (est_entier "0" 10) true
let () = test_unitaire_assert (est_entier "" 10) false
let () = test_unitaire_assert (est_entier "123a" 10) false
let () = test_unitaire_assert (est_entier "_123" 10) false
let () = test_unitaire_assert (est_entier "1u23" 10) false

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

let () = test_unitaire_assert (est_variable "a") true
let () = test_unitaire_assert (est_variable "x") true
let () = test_unitaire_assert (est_variable "x'") true
let () = test_unitaire_assert (est_variable "pi") true
let () = test_unitaire_assert (est_variable "pi2") true
let () = test_unitaire_assert (est_variable "pi-2") false
let () = test_unitaire_assert (est_variable "pi_t") true
let () = test_unitaire_assert (est_variable "pi_2") true
let () = test_unitaire_assert (est_variable "a_") false
let () = test_unitaire_assert (est_variable "Ax") false (*Constante qui commence par une maj ? *)
let () = test_unitaire_assert (est_variable "x'''''") true
let () = test_unitaire_assert (est_variable "x'''''123") true



;;
let () =
  Printf.printf "%3.2f %% of tests passed" (100. *. float_of_int !test_valide /. float_of_int !compteur_de_test) in
let () = compteur_de_test := 0 in
test_valide := 0
;;
