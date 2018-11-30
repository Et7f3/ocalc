open String

let print_bool b =
  print_string (
    if b then
      "true"
    else
      "false"
  )

let in_array e =
  let rec in_array = function
      a :: l when a = e -> true
    | [] -> false
    | a :: l -> in_array l
  in in_array

let str_include s l =
  let len = String.length s in
  let rec si p(*arenthesis*) b(*rackets*) b'(*races*) i(*index*) =
    if i = len then
      false
    else
       match s.[i] with
        '(' -> si (p + 1) b b' (i + 1)
      | ')' -> si (p - 1) b b' (i + 1)
      | '{' -> si p (b + 1) b' (i + 1)
      | '}' -> si p (b - 1) b' (i + 1)
      | '[' -> si p b (b' + 1) (i + 1)
      | ']' -> si p b (b' - 1) (i + 1)
      | c when in_array c l && p = b && b = b' && b' = 0 -> in_array s.[i] l || si p b  b' (i + 1)
      | _ -> si p b b' ( i + 1)
  in si 0 0 0 0

let is_int s b =
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

type bigint = int list

type bigfloat = int list * int list

type expr =
    Vide
  | Valeure_entiere of bigint (* store -126 *)
  | Valeure_reelle of bigfloat (* store 123.5 *)
  | Variable of string (* store pi or x *)
  | Fonction of string * expr list (* store f12346'(x, y) *) (* name statrt with 'a'..'z' 'A'..'Z' then '0'..'9' '\'' 'a'..'z' 'A'..'Z' *) (*parameters are comma semarated *)
  | Parenthese of expr (* store (5+9) and expr = 5+9 *)
  | UnparsedString of string (*should not be used when AST is completed *)
  | Addition of expr * expr
  | Soustraction of expr * expr
  | Multiplication of expr * expr (* space are like multiplication so "x pi" = "x*pi" = Multiplication (x, pi) *)
  | Division of expr * expr;;

let bigint_of_string s =
  (*let sign =
    if sign = '-' then
      -1
    else
      1
  in *)let s =
    if '0' <= s.[0] && s.[0] <= '9' then
      s
    else
      String.sub s 1 (String.length s - 1)
  in let rec p s =
       let l = String.length s in
       if l = 0 then
         []
       else
         match s.[String.length s - 1] with
           ('0' .. '9') as c -> int_of_char c - 48 :: p (String.sub s 0 (String.length s - 1))
         | _ -> failwith "not a bigint"
  in p s

let rec explode_string s l =
  let len = String.length s in
  let s = s in
  let rec ex p(*arenthesis*) b(*rackets*) b'(*races*) i(*index*) =
    if i = len then
      [String.sub s 0 i]
    else
      match s.[i] with
        '(' -> ex (p + 1) b b' (i + 1)
      | ')' -> ex (p - 1) b b' (i + 1)
      | '{' -> ex p (b + 1) b' (i + 1)
      | '}' -> ex p (b - 1) b' (i + 1)
      | '[' -> ex p b (b' + 1) (i + 1)
      | ']' -> ex p b (b' - 1) (i + 1)
      | c when in_array c l && p = b && b = b' && b' = 0 -> String.sub s 0 i :: String.sub s i 1 :: explode_string (String.sub s (i + 1) (len - i - 1)) l
      | _ -> ex p b b' ( i + 1)
  in ex 0 0 0 0;;



let rec expr_of_string s =
  if s = "" then
    Vide
  else if is_int s 10 then
    Valeure_entiere (bigint_of_string s)
  else if str_include s ['+'] then
    let rec recompose = function
        a :: "+" :: l -> Addition (expr_of_string a, recompose l)
      | _ :: _ :: _ -> failwith "Operateur manquant"
      | a :: [] -> expr_of_string a
      | [] -> Vide
    in recompose (explode_string s ['+'])
  else if str_include s ['-'] then
    let rec recompose = function
        a :: "-" :: l -> Soustraction (expr_of_string a, recompose l)
      | _ :: _ :: _ -> failwith "Operateur manquant"
      | a :: [] -> expr_of_string a
      | [] -> Vide
    in recompose (explode_string s ['-'])
  else
    UnparsedString (s)

(*
let a  = is_int "03" 3;;

let a = is_int "0879" 10;;

let a = is_int_base "(03)_3";;

let a = is_int_base "(0879)_10";;

let a = is_int_base "(26)";;
*)

(*
let s = "+129+16+3698-49+6";;

let s1 = "+159";;
let s2 = "-129+16+3698-(49+6)";;

(*
let a = str_include s ['+'; '-'];;
*)

let a = expr_of_string s;;

let a = expr_of_string s1;;

let a = expr_of_string s2;;

let a = explode_string s2 ['+'; '-'];;

(*
let a = explode_string s ['+'; '-'];;
*)
*)
;;








let rec explode_string s l =
  let len = String.length s in
  let s = s in
  let rec ex p(*arenthesis*) b(*rackets*) b'(*races*) i(*index*) =
    if i = len then
      [String.sub s 0 i]
    else
      match s.[i] with
        '(' -> ex (p + 1) b b' (i + 1)
      | ')' -> ex (p - 1) b b' (i + 1)
      | '{' -> ex p (b + 1) b' (i + 1)
      | '}' -> ex p (b - 1) b' (i + 1)
      | '[' -> ex p b (b' + 1) (i + 1)
      | ']' -> ex p b (b' - 1) (i + 1)
      | c when in_array c l && p = b && b = b' && b' = 0 ->
        String.sub s 0 i :: String.sub s i 1 :: explode_string (String.sub s (i + 1) (len - i - 1)) l
      | _ -> ex p b b' ( i + 1)
  in ex 0 0 0 0;;












let rec list_to_expr expr = function
    [] -> expr
  | e :: [] -> invalid_arg "error"
  | "+" :: e :: l -> list_to_expr (Addition (expr, UnparsedString e)) l
  | "-" :: e :: l -> list_to_expr (Soustraction (expr, UnparsedString e)) l;;
let f x =
  match x with
    [] -> invalid_arg "empty_list"
  | e :: l -> list_to_expr (UnparsedString e) l;;
let f x =
  match x with
    [] -> invalid_arg "empty_list"
  | "" :: l -> list_to_expr Vide l
  | e :: l -> list_to_expr (UnparsedString e) l;;
let rec f x =
  match x with
    [] -> Vide
  | "" :: "" :: l -> 
f (explode_string "+129+16+3698-49+6" ['+'; '-']);;
f (explode_string "-129+16+3698-49+6" ['+'; '-']);;
f (explode_string "129+16+3698-49+6" ['+'; '-']);;
f (explode_string "" ['+'; '-']);;

(* +129+16+3698-49+6
 * Addition (129, Addition (16, Addition (3698, soustraction (-49,6))))
 * Addition (Soustraction (Addition (Addition (129, 16), 3698), 49), 6)
 * "" :: "+" :: "129" :: "+" :: "16" :: "+" :: "3698" :: "-" :: "49" :: "+" :: "6"
 * 129 16 + 3698 + 49 - 6 +
 *
 *
 *
 * *)
