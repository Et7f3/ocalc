open Type
open Utils

let est_entier t(*exte*) b(*ase*) =
  let l(*ongueur*) = String.length t in
  let m(*aximum caractere *) = char_of_int (b + 48) in
  let rec est_entier i =
    i = l
    || '0' <= t.[i]
       && t.[i] < m
       && est_entier (i + 1)
  in    2 <= l
        && ('-' = t.[0]
            || '+' = t.[0])
        && est_entier 1
        || 1 <= l
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
      && let b = int_of_string (String.sub t (i + 1) (l - i - 1)) in
      0 <= b
      && b < 11
      && (t.[0] = '('
          && t.[i - 1] = ')'
          && est_entier (String.sub t 1 (i - 2)) b)
      || (t.[0] != '('
          && t.[i - 1] != ')'
          && est_entier (String.sub t 0 i) b)

let est_variable t(*exte*) =
  let l(*ongueur*) = String.length t in
  let rec est_variable i =
    i = l
    || 'a' <= t.[i] && t.[i] <= 'z'
       && est_variable2 (i + 1)
  and est_variable2 i =
    i = l
    || match t.[i] with
    '_' -> i + 1 <> l && est_variable2 (i + 1)
    | c when 'a' <= c && c <= 'z' -> est_variable2 (i + 1)
    | c when 'A' <= c && c <= 'Z' -> est_variable2 (i + 1)
    | c when '0' <= c && c <= '9' -> est_variable2 (i + 1)
    | '\'' -> est_variable2 (i + 1)
    | _ -> false
  in est_variable 0
