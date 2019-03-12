open Type
open Utils
open Lexer
open Parser
open GrandEntier_on


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

let () = test_unitaire_assert (contient_texte "123" ['+']) false
let () = test_unitaire_assert (contient_texte "123+456" ['+']) true
let () = test_unitaire_assert (contient_texte "123+456" ['-']) false
let () = test_unitaire_assert (contient_texte "123-456+789" ['-'; '+']) true

let () = fin_suite ()

let () = init_suite "couper_texte"

let () = test_unitaire_assert (couper_texte "123" ['+']) ["123"]
let () = test_unitaire_assert (couper_texte "123+456" ['+']) ["123"; "+"; "456"]
let () = test_unitaire_assert (couper_texte "123+456" ['-']) ["123+456"]
let () = test_unitaire_assert (couper_texte "123-456+789" ['-'; '+']) ["123"; "-"; "456"; "+"; "789"]

let () = fin_suite ()

let () = init_suite "texte_de_expr"

let () = test_unitaire_assert (texte_de_expr (Variable "x")) "x"
let () = test_unitaire_assert (texte_de_expr (Operation ("+", [Variable "x"]))) "x"
let () = test_unitaire_assert (texte_de_expr (Operation ("+", [Variable "x"; Variable "y"]))) "(x+y)"
let () = test_unitaire_assert (texte_de_expr (Operation ("*", [Operation ("+", [Variable "x"; Variable "y"])]))) "(x+y)"
let () = test_unitaire_assert (texte_de_expr (Operation ("*", [Variable "z"; Operation ("+", [Variable "x"; Variable "y"])]))) "(z*(x+y))"

let () = fin_suite ()

let () = init_suite "expr_de_texte_etend"

let parse = []
(* this is an old time
   let () = test_unitaire_assert (expr_de_texte_etend parse "3445") (Textenonvalide  "3445") in
   let () = test_unitaire_assert (expr_de_texte_etend parse "x") (Textenonvalide  "x") in
*)
let parse =
  let parse = (est_addition_soustraction, variable_de_addition_soustraction) :: parse in
  let parse = (est_variable, variable_de_texte) :: parse in
  let parse = (est_entier10, variable_de_entier) :: parse in
  parse
let () = test_unitaire_assert (expr_de_texte_etend parse "3445") (Entier (ge "3445"))
let () = test_unitaire_assert (expr_de_texte_etend parse "+3445") (Entier (ge "3445"))
let () = test_unitaire_assert (expr_de_texte_etend parse "-3445") (Entier (ge "-3445"))
let () = test_unitaire_assert (expr_de_texte_etend parse "x+3445") (Operation ("+", [Variable "x"; Entier (ge "3445")]))
let () = test_unitaire_assert (expr_de_texte_etend parse "x+3445-y") (Operation ("+", [Variable "x"; Entier (ge "3445"); Neg (Variable "y")]))
let () = test_unitaire_assert (expr_de_texte_etend parse "-123+x-y") (Operation ("+", [Entier (ge "-123"); Variable "x"; Neg (Variable "y")]))
let () = test_unitaire_assert (expr_de_texte_etend parse "-z") (Neg (Variable "z"))
let () = test_unitaire_assert (expr_de_texte_etend parse "+alexandre") (Variable "alexandre")
let () = test_unitaire_assert (expr_de_texte_etend parse "x") (Variable ("x"))

let () = fin_suite ()

(** test sur les Grand Entiers *)
let grandentier_depuis_texte a = Obj.magic grandentier_depuis_texte a
let texte_depuis_grandentier a = Obj.magic texte_depuis_grandentier a
let a = grandentier_depuis_texte "-5"
let b = grandentier_depuis_texte "-50"
let c = grandentier_depuis_texte "5"
let d = grandentier_depuis_texte "0"
let e = grandentier_depuis_texte "-0"

let () = init_suite "est_negatif"

let () = test_unitaire_assert (est_negatif a) true
let () = test_unitaire_assert (est_negatif b) true
let () = test_unitaire_assert (est_negatif c) false
let () = test_unitaire_assert (est_negatif d) false
let () = test_unitaire_assert (est_negatif e) false

let () = fin_suite ()

let () = init_suite "comparer"

let () = test_unitaire_assert (comparer a b) ~-1
let () = test_unitaire_assert (comparer b a) 1
let () = test_unitaire_assert (comparer c a) ~-1
let () = test_unitaire_assert (comparer a c) 1
let () = test_unitaire_assert (comparer a d) 1
let () = test_unitaire_assert (comparer d e) 0

let () = fin_suite ()

let () = init_suite "grandentier_depuis_texte"

let () = test_unitaire_assert (grandentier_depuis_texte "3445") (false, [5; 4; 4; 3])
let () = test_unitaire_assert (grandentier_depuis_texte "-3445") (true, [5; 4; 4; 3])
let () = test_unitaire_assert (grandentier_depuis_texte "0") (false, [])
let () = test_unitaire_assert (grandentier_depuis_texte "-0") (false, [])

let () = fin_suite ()

let () = init_suite "texte_depuis_grandentier"

let () = test_unitaire_assert (texte_depuis_grandentier (false, [5; 4; 4; 3])) "3445"
let () = test_unitaire_assert (texte_depuis_grandentier (true, [5; 4; 4; 3])) "-3445"
let () = test_unitaire_assert (texte_depuis_grandentier (false, [])) "0"
let () = test_unitaire_assert (texte_depuis_grandentier (true, [])) "0"

let () = fin_suite ()

let () = Printf.printf "\n\n%3.2f %% des tests global passés\n" (100. *. float_of_int !test_valide_total /. float_of_int !test_total)
let () = reinit_test_suite ()
