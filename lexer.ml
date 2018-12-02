let (compteur_de_test, test_valide) = ref 0, ref 0

let rec test_unitaire_assert a b =
  let () =
    if a <> b then
      Printf.printf "Le test #%d n'as pas produit la valeure attendue\n" !compteur_de_test
    else
      test_valide := !test_valide + 1
  in compteur_de_test := !compteur_de_test + 1

let est_entier t(*exte*) b(*ase*) =
  let l(*ongueur*) = String.length t in
  let m(*aximum caractere *) = char_of_int (b + 48) in
  let rec est_entier i =
      i = l
    ||
        '0' <= t.[i]
      &&
        t.[i] < m
      &&
        est_entier (i + 1)
  in    2 <= l
      &&
          ('-' = t.[0]
        ||
          '+' = t.[0])
      &&
        est_entier 1
    ||
        1 <= l
      &&
        est_entier 0

let est_entier_base t(*exte*) =
  let l(*ongueur*) = String.length t in
  let i =
    try
      String.index t '_'
    with Not_found -> -1
  in
      i > 0
    &&
      let b = int_of_string (String.sub t (i + 1) (l - i - 1)) in
        0 <= b
      &&
        b < 11
    &&
          (t.[0] = '('
        &&
          t.[i - 1] = ')'
        &&
          est_entier (String.sub t 1 (i - 2)) b)
      ||
          (t.[0] != '('
        &&
          t.[i - 1] != ')'
        &&
          est_entier (String.sub t 0 i) b)

let est_variable s = false

(*
let parenthese_correcte t(*exte*) =
  let l(*onguer*) = String.length t in
  let rec p_c o(*uvertes*) i(*ndex*) =
    match t.[i] with
      '(' | '{' | '[' as c -> o = [] && i + 1 = l || p_c (c :: o) (i + 1)
    | ')' | '}' | ']' as c -> o = [] && i + 1 = l || p_c (c :: o) (i + 1)
    | _ -> expr2
  in l > 2 && p_c [] 0
*)


;;


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






(*
let is_int? s(*tring *) b(*base*) =
  let l = String.length s in
  let max_char = char_of_int (b + 48) in
  let rec is_int i =
    if i = l then
      true
    else
      match s.[i] with
        c when '0' <= c && c < max_char -> is_int (i + 1)
      | _ -> false
  in is_int 0

let is_int_base s =
  let l = String.length s in
  let i =
    try
      String.index s '_' + 1
    with Not_found -> 0
  in i != 0 && 5 <= l && s.[0] = '(' && s.[i - 2] = ')' && is_int (String.sub s 1 (l - i + 2)) (int_of_string (String.sub s i (l - i)))
*)

