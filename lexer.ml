open Type
open Utils

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

let est_entier10 t = est_entier t 10

let est_entier_base t(*exte*) =
  let l(*ongueur*) = String.length t in
  let i =
    try
      String.index t '_'
    with Not_found -> -1
  in  i > 0
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

let est_variable t(*exte*) =
  let l(*ongueur*) = String.length t in
  let rec est_variable i =
      i = l
    ||
        'a' <= t.[i]
      &&
        t.[i] <= 'z'
      &&
        est_variable2 (i + 1)
  and est_variable2 i =
      i = l
    ||
        t.[i] = '_'
      &&
        i + 1 <> l
      &&
        est_variable2 (i + 1)
    ||
        ('a' <= t.[i]
      &&
        t.[i] <= 'z')
      &&
        est_variable2 (i + 1)
    ||
        ('A' <= t.[i]
      &&
        t.[i] <= 'Z')
      &&
        est_variable2 (i + 1)
    ||
        ('0' <= t.[i]
      &&
        t.[i] <= '9')
      &&
        est_variable2 (i + 1)
    ||
        t.[i] = '\''
      &&
        est_variable2 (i + 1)
  in est_variable 0 (*Maybe add a match with *)

let contient_texte t(*exte*) l(*iste à chercher*) = false

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
