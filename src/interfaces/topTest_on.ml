open Type
open Utils
open Lexer
open Parser
open GrandEntier_on

let main (argc, argv) =
  (*numéro de *)
  let (test_total, test_valide_total, suite, test, test_valide) = ref 0, ref 0, ref 0, ref 0, ref 0 in

  let rec test_unitaire_assert a b =
    let () =
      if a <> b then
        Printf.printf "Le test #%d n'as pas produit la valeure attendue\n" !test
      else
        let () = test_valide := !test_valide + 1 in
        test_valide_total := !test_valide_total + 1
    in let () = test_total := !test_total + 1 in
    test := !test + 1 in

  let init_suite name =
    let () = Printf.printf "\n\n\x1b[4mSuite #%d\x1b[0m: %s\n" !suite name in
    suite := !suite + 1 in

  let fin_suite () =
    let () =
      Printf.printf "%3.2f %% de tests passés pour cette suite\n" (100. *. float_of_int !test_valide /. float_of_int !test) in
    let () = test_valide := 0 in
    test := 0 in

  let reinit_test_suite () =
    let () = test_total := 0 in
    let () = test_valide_total := 0 in
    let () = suite := 0 in
    let () = test := 0 in
    test_valide := 0 in


  let () = init_suite "est_entier" in

  let () = test_unitaire_assert (est_entier "123" 10) true in
  let () = test_unitaire_assert (est_entier "-123" 10) true in
  let () = test_unitaire_assert (est_entier "+123" 10) true in
  let () = test_unitaire_assert (est_entier "007" 10) true in
  let () = test_unitaire_assert (est_entier "0" 10) true in
  let () = test_unitaire_assert (est_entier "" 10) false in
  let () = test_unitaire_assert (est_entier "123a" 10) false in
  let () = test_unitaire_assert (est_entier "_123" 10) false in
  let () = test_unitaire_assert (est_entier "1u23" 10) false in

  let () = fin_suite () in

  let () = init_suite "est_entier_base" in

  let () = test_unitaire_assert (est_entier_base "12310") false in
  let () = test_unitaire_assert (est_entier_base "123_10") true in
  let () = test_unitaire_assert (est_entier_base "(123)_10") true in
  let () = test_unitaire_assert (est_entier_base "123)_10") false in
  let () = test_unitaire_assert (est_entier_base "(123_10") false in
  let () = test_unitaire_assert (est_entier_base "_123_10") false in
  let () = test_unitaire_assert (est_entier_base "g123_10") false in
  let () = test_unitaire_assert (est_entier_base "123u_10") false in
  let () = test_unitaire_assert (est_entier_base "12b3_10") false in
  let () = test_unitaire_assert (est_entier_base "à12b3_10") false in

  let () = fin_suite () in

  let () = init_suite "est_variable" in

  let () = test_unitaire_assert (est_variable "a") true in
  let () = test_unitaire_assert (est_variable "x") true in
  let () = test_unitaire_assert (est_variable "x'") true in
  let () = test_unitaire_assert (est_variable "pi") true in
  let () = test_unitaire_assert (est_variable "pi2") true in
  let () = test_unitaire_assert (est_variable "pi-2") false in
  let () = test_unitaire_assert (est_variable "pi_t") true in
  let () = test_unitaire_assert (est_variable "pi_2") true in (*Va être confondu avec est_entier_base ?*)
  let () = test_unitaire_assert (est_variable "a_") false in (*Termine par un _ *)
  let () = test_unitaire_assert (est_variable "Ax") false in (*Constante qui commence par une maj ? Matrice ?*)
  let () = test_unitaire_assert (est_variable "x'''''") true in (*apostrophe ok*)
  let () = test_unitaire_assert (est_variable "x'''''123") true in
  let () = test_unitaire_assert (est_variable "xZE") true in (*Majuscule pas à la première ok*)

  let () = fin_suite () in

  let () = init_suite "contient_texte" in

  let () = test_unitaire_assert (contient_texte "123" ['+']) false in
  let () = test_unitaire_assert (contient_texte "123+456" ['+']) true in
  let () = test_unitaire_assert (contient_texte "123+456" ['-']) false in
  let () = test_unitaire_assert (contient_texte "123-456+789" ['-'; '+']) true in

  let () = fin_suite () in

  let () = init_suite "couper_texte" in

  let () = test_unitaire_assert (couper_texte "123" ['+']) ["123"] in
  let () = test_unitaire_assert (couper_texte "123+456" ['+']) ["123"; "+"; "456"] in
  let () = test_unitaire_assert (couper_texte "123+456" ['-']) ["123+456"] in
  let () = test_unitaire_assert (couper_texte "123-456+789" ['-'; '+']) ["123"; "-"; "456"; "+"; "789"] in

  let () = fin_suite () in

  let () = init_suite "texte_de_expr" in

  let () = test_unitaire_assert (texte_de_expr (Variable "x")) "x" in
  let () = test_unitaire_assert (texte_de_expr (Operation ("+", [Variable "x"]))) "x" in
  let () = test_unitaire_assert (texte_de_expr (Operation ("+", [Variable "x"; Variable "y"]))) "(x+y)" in
  let () = test_unitaire_assert (texte_de_expr (Operation ("*", [Operation ("+", [Variable "x"; Variable "y"])]))) "(x+y)" in
  let () = test_unitaire_assert (texte_de_expr (Operation ("*", [Variable "z"; Operation ("+", [Variable "x"; Variable "y"])]))) "(z*(x+y))" in

  let () = fin_suite () in

  let () = init_suite "expr_de_texte_etend" in

  let parse = [] in
  let ge = Parser.ge in
  (* this is an old time
  let () = test_unitaire_assert (expr_de_texte_etend parse "3445") (Textenonvalide  "3445") in
 let () = test_unitaire_assert (expr_de_texte_etend parse "x") (Textenonvalide  "x") in
  *)
  let parse =
    let parse = (est_addition_soustraction, variable_de_addition_soustraction) :: parse in
    let parse = (est_variable, variable_de_texte) :: parse in
    let parse = (est_entier10, variable_de_entier) :: parse in
    parse in
  let () = test_unitaire_assert (expr_de_texte_etend parse "3445") (Entier ge) in
  let () = test_unitaire_assert (expr_de_texte_etend parse "+3445") (Entier ge) in
  let () = test_unitaire_assert (expr_de_texte_etend parse "-3445") (Entier ge) in
  let () = test_unitaire_assert (expr_de_texte_etend parse "x+3445") (Operation ("+", [Variable "x"; Entier ge])) in
  let () = test_unitaire_assert (expr_de_texte_etend parse "x+3445-y") (Operation ("+", [Variable "x"; Entier ge; Neg (Variable "y")])) in
  let () = test_unitaire_assert (expr_de_texte_etend parse "-123+x-y") (Operation ("+", [Neg (Entier ge); Variable "x"; Neg (Variable "y")])) in
  let () = test_unitaire_assert (expr_de_texte_etend parse "-z") (Neg (Variable "z")) in
  let () = test_unitaire_assert (expr_de_texte_etend parse "+alexandre") (Variable "alexandre") in
  let () = test_unitaire_assert (expr_de_texte_etend parse "x") (Variable ("x")) in

  let () = fin_suite () in

  (*let () = init_suite "grandentier_depuis_texte" in

    let () = test_unitaire_assert (grandentier_depuis_texte "3445") (false, [5; 4; 4; 3]) in
    let () = test_unitaire_assert (grandentier_depuis_texte "-3445") (true, [5; 4; 4; 3]) in
    let () = test_unitaire_assert (grandentier_depuis_texte "0") (false, []) in
    let () = test_unitaire_assert (grandentier_depuis_texte "-0") (false, []) in

    let () = fin_suite () in

    let () = init_suite "texte_depuis_grandentier" in

    let () = test_unitaire_assert (texte_depuis_grandentier (false, [5; 4; 4; 3])) "3445" in
    let () = test_unitaire_assert (texte_depuis_grandentier (true, [5; 4; 4; 3])) "-3445" in
    let () = test_unitaire_assert (texte_depuis_grandentier (false, [])) "0" in
    let () = test_unitaire_assert (texte_depuis_grandentier (true, [])) "0" in

    let () = fin_suite () in*)

  let () = Printf.printf "\n\n%3.2f %% des tests global passés\n" (100. *. float_of_int !test_valide_total /. float_of_int !test_total) in
  let () = reinit_test_suite () in
  ()
