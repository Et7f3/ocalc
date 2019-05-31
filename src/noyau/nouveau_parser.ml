type expression =
    Vide
  | Integer of string
  | Reel of string
  | Var of string
  | Tuple of int * expression list
  | Fonction of string * int * expression list
  | Operation of [ `Multiplication | `Addition ] * expression list
  | Inv of expression
  | Neg of expression

type affectable =
    Pas_affectable
  | Def_Var of string
  | Def_fonction of string * string list

type entre_valide =
    Erreur of string * Nouveau_lexer.token list
  | Expression of expression
  | Definition of (affectable * expression) list

let number =
  let open Nouveau_lexer in
  let rec loop = function
      e1, Digit e2 :: l | e1, Blanc :: Digit e2 :: l -> loop (e1 ^ e2, l)
    | e, Souligne :: l -> loop (e, l)
    | e, l -> Integer e, l
  in function
      (* (Operateur Plus) :: Digit e :: l | *) (* + number will be treted later *)
      Digit e :: l -> loop (e, l)
    | l -> Vide, l

(*let reel l =
  let open Nouveau_lexer in
  match number l with
    Integer e1, Separateur_unite :: l -> (
      match number l with
        Integer e2, l -> Reel (e1 ^ "," ^ e2), l
      | Vide, l -> Integer e1, l
      | e, l -> Vide, l
      )
  | Vide, Separateur_unite :: l -> (
    match number l with
      Integer e, l -> Reel ("," ^ e), l
    | e, l -> Reel ("0,0"),  l (* , = 0,0 *)
    )
  | _ -> Vide, l*)

let reel l =
  let open Nouveau_lexer in
  let e1, l =
    match number l with
      Integer e, l -> Integer e, l
    | _ -> Vide, l
  in
  let virgule, e2, l =
    match l with
      Separateur_unite :: l -> (
          match number l with
            Integer e, l -> true, Integer e, l
          | _ -> true, Vide, l
        )
    | l -> false, Vide, l
  in
    match e1, e2 with
      Integer e1, Integer e2 -> Reel (e1 ^ "," ^ e2), l
    | Integer e, Vide -> Integer e, l
    | Vide, Integer e -> Reel ("," ^ e), l
    | Vide, Vide when virgule -> Reel ",", l
    | _, _ -> Vide, l

let variable =
  let open Nouveau_lexer in
  function
      Identifiant i :: l -> Var i, l
    | l -> Vide, l

let tuple_de f l =
  let open Nouveau_lexer in
  let rec loop n acc = (function
  | Parenthese_fermante :: l -> n, List.rev acc, l
  | Separateur_liste :: l -> (
    match f l with
    | Vide, _ -> 1, [Vide], l
    | e, l -> loop (n + 1) (e :: acc) l
    )
  | l ->
    let () = prerr_endline "Parenthese_fermante manquante" in
    1, [Vide], l
  )
  in match l with
    (Parenthese_ouvrante :: l) as original_liste -> (
      match f l with
      | Vide, _ -> Vide, original_liste
      | e, l -> (
        match loop 1 [e] l with
          1, [e], l -> e, l
        | n, e, l -> Tuple (n, e), l
        )
      )
  | l -> Vide, l

let rec fonction l =
  let open Nouveau_lexer in
  match l with
    (Identifiant f :: l) as original_liste -> (
      match tuple_de expression l with
      | Tuple (n, liste_de_parametres), l -> Fonction (f, n, liste_de_parametres), l
      | x, l when x <> Vide -> Fonction (f, 1, [x]), l
      | _, _ -> Vide, original_liste (* c'est forcement Vide *)
      )
  | l -> Vide, l

and facteur l =
  let open Nouveau_lexer in
  match reel l with
    Vide, _ -> (
      match number l with
        Vide, _ -> (
          match fonction l with
            Vide, _ -> (
              match variable l with
                Vide, _ -> tuple_de expression l
              | e, l -> e, l
              )
          | e, l -> e, l
          )
      | e, l -> e, l
      )
  | e, l -> e, l

and terme l =
  let open Nouveau_lexer in
  let rec loop acc = function
      (Operateur Fois | Blanc) :: l -> (
          match facteur l with
            Vide, _ -> [Vide], l
          | e, l -> loop (e :: acc) l
        )
    | Operateur Division :: l -> (
        match facteur l with
          Vide, _ -> [Vide], l
        | e, l -> loop (Inv e :: acc) l
      )
    | l -> List.rev acc, l
  in let inverse, l =
    match l with
      Operateur Division :: l -> true, l
    | Operateur Fois :: l | l -> false, l
  in match facteur l with
    Vide, _ -> Vide, l (* todo error handling*)
  | e, l ->
    match loop [if inverse then Inv e else e] l with
      [e], l -> e, l
    | e, l -> Operation (`Multiplication, e), l

and expression l =
  let open Nouveau_lexer in
  let rec loop acc l =
    match l with
      Operateur Plus :: l -> (
          match terme l with
            Vide, _ -> [Vide], l
          | e, l -> loop (e :: acc) l
        )
    | Operateur Moins :: l -> (
        match terme l with
          Vide, _ -> [Vide], l
        | e, l -> loop (Neg e :: acc) l
      )
    | l -> List.rev acc, l
  in let negatif, l =
    match l with
      Operateur Moins :: l -> true, l
    | Operateur Plus :: l | l -> false, l
  in match terme l with
    Vide, _ -> Vide, l (* todo error handling*)
  | e, l ->
    match loop [if negatif then Neg e else e] l with
      [e], l -> e, l
    | e, l -> Operation (`Addition, e), l

(*let definition_simple l =
  let open Nouveau_lexer in
  match l with
    (Identifiant i :: Operateur Definition :: l) as original_liste -> (
      match expression l with
        Vide, _ -> Vide, original_liste
      | e, l -> Def ([Var i], [e]), l
      )
  | l -> Vide, l

let expression_principale l =
  match definition_simple l with
    Vide, _ -> (match expression l with
      Vide, _ -> Vide, l
    | e, [] -> e, []
    | e, l -> Vide, l)
  | e, [] -> e, []
  | e, l -> Vide, l*)

let affectable l =
  match fonction l with
    Vide, _ -> variable l
  | e, l -> e, l

let tuple_d'affectable = tuple_de affectable

let est_affectable e =
  match e with
    Var x -> true
  | Fonction (_, _, l) -> List.for_all (fun e -> match e with Var _ -> true | _ -> false) l
  | _ -> false

let tous_affectable e =
  match e with
    Tuple (_, l) -> List.for_all est_affectable l
  | e -> est_affectable e

let exrp_l_to_string_l l = List.map (fun e -> match e with Var x -> x | _ -> "error") l

let expr_to_def = function
    Var x, e -> Def_Var x, e
  | Fonction (name, _, l), e -> Def_fonction (name, exrp_l_to_string_l l), e
  | _, _ -> Pas_affectable, Vide

let exprs_to_def = function
    Var x, e -> Definition [Def_Var x, e]
  | Fonction (name, _, l), e -> Definition [Def_fonction (name, exrp_l_to_string_l l), e]
  | Tuple(a, la), Tuple(b, lb) ->
    if a = b then
      Definition (List.map2 (fun a b -> expr_to_def (a, b)) la lb)
    else
      Erreur ("nombre de membre incorrect", [])
  | _, _ -> Erreur ("WTF", [])

let expression_principale l =
  match expression l with
    e, Nouveau_lexer.Operateur Nouveau_lexer.Definition :: l -> (
        if tous_affectable e then
          match expression l with
            Vide, l -> Erreur ("membre droit incorrect", [])
          | e2, [] -> exprs_to_def (e, e2)
          | _, l -> Erreur ("caracteres restant", l)
        else
          Erreur ("membre de gauche non assignable", [])
      )
  | e, [] -> Expression e
  | e, l -> Erreur ("caracteres restant", l)


let filtre =
  let open Nouveau_lexer in
  let rec filtre acc =
    function
      Blanc :: [] | [] -> List.rev acc
    | Blanc :: (Operateur v) :: l | (Operateur v) :: Blanc :: l -> filtre acc ((Operateur v) :: l)
    | Blanc :: Separateur_liste :: l | Separateur_liste :: Blanc :: l -> filtre acc (Separateur_liste :: l)
    | Parenthese_ouvrante :: Blanc :: l -> filtre acc (Parenthese_ouvrante :: l)
    | Blanc :: Parenthese_fermante :: l -> filtre acc (Parenthese_fermante :: l)
    | Blanc :: Separateur_unite :: l -> filtre acc (Separateur_unite :: l)
    | Separateur_unite :: Blanc :: Digit e :: l -> filtre acc (Separateur_unite :: Digit e :: l)
    | e :: l -> filtre (e :: acc) l
  in function
    Blanc :: l -> filtre [] l
  | l -> filtre [] l

let parse s =
  let lexbuf = Nouveau_lexer.to_lexbuf s in
  let lexbuf = filtre lexbuf in
expression_principale lexbuf
