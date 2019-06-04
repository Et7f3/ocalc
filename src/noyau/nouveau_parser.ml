type expression =
    Vide
  | Entier of string
  | Reel of string
  | Variable of string
  | Tuple of int * expression list
  | Fonction of string * int * expression list
  | Operation of [ `Multiplication | `Addition ] * expression list
  | Inverse of expression
  | Negation of expression

type affectable =
    Pas_affectable
  | Definition_Variable of string
  | Definition_fonction of string * string list

type entre_valide =
    Erreur of string * Nouveau_lexer.token list
  | Expression of expression
  | Definition of (affectable * expression) list

let entier =
  let open Nouveau_lexer in
  let rec boucle = function
      e1, Digit e2 :: l | e1, Blanc :: Digit e2 :: l -> boucle (e1 ^ e2, l)
    | e, Souligne :: l -> boucle (e, l)
    | e, l -> Entier e, l
  in function
      (* (Operateur Plus) :: Digit e :: l | *) (* + entier sera traité plus tard *)
      Digit e :: l -> boucle (e, l)
    | l -> Vide, l

let reel l =
  let open Nouveau_lexer in
  let e1, l =
    match entier l with
      Entier e, l -> Entier e, l
    | _ -> Vide, l
  in
  let virgule, e2, l =
    match l with
      Separateur_unite :: l -> (
          match entier l with
            Entier e, l -> true, Entier e, l
          | _ -> true, Vide, l
        )
    | l -> false, Vide, l
  in
    match e1, e2 with
      Entier e1, Entier e2 -> Reel (e1 ^ "," ^ e2), l
    | Entier e, Vide -> Entier e, l
    | Vide, Entier e -> Reel ("," ^ e), l
    | Vide, Vide when virgule -> Reel ",", l
    | _, _ -> Vide, l

let variable =
  let open Nouveau_lexer in
  function
      Identifiant i :: l -> Variable i, l
    | l -> Vide, l

let tuple_de f l =
  let open Nouveau_lexer in
  let rec boucle n acc = (function
  | Parenthese_fermante :: l -> n, List.rev acc, l
  | Separateur_liste :: l -> (
    match f l with
    | Vide, _ -> 1, [Vide], l
    | e, l -> boucle (n + 1) (e :: acc) l
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
        match boucle 1 [e] l with
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
  match reel l with
    Vide, _ -> (
      match entier l with
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
  let rec boucle acc = function
      (Operateur Fois | Blanc) :: l -> (
          match facteur l with
            Vide, _ -> [Vide], l
          (*| (Entier _ | Reel _) as e, Identifiant i :: l -> boucle (Variable i :: e :: acc) l*)
          | e, l -> boucle (e :: acc) l
        )
    | Operateur Division :: l -> (
        match facteur l with
          Vide, _ -> [Vide], l
        | e, l -> boucle (Inverse e :: acc) l
      )
    | l -> List.rev acc, l
  in let inverse, l =
    match l with
      Operateur Division :: l -> true, l
    | Operateur Fois :: l | l -> false, l
  in match facteur l with
    Vide, _ -> Vide, l (* todo error handling*)
  | e, l ->
    match boucle [if inverse then Inverse e else e] l with
      [e], l -> e, l
    | e, l -> Operation (`Multiplication, e), l

and expression l =
  let open Nouveau_lexer in
  let rec boucle acc l =
    match l with
      Operateur Plus :: l -> (
          match terme l with
            Vide, _ -> [Vide], l
          | e, l -> boucle (e :: acc) l
        )
    | Operateur Moins :: l -> (
        match terme l with
          Vide, _ -> [Vide], l
        | e, l -> boucle (Negation e :: acc) l
      )
    | l -> List.rev acc, l
  in let negatif, l =
    match l with
      Operateur Moins :: l -> true, l
    | Operateur Plus :: l | l -> false, l
  in match terme l with
    Vide, _ -> Vide, l (* todo error handling*)
  | e, l ->
    match boucle [if negatif then Negation e else e] l with
      [e], l -> e, l
    | e, l -> Operation (`Addition, e), l

let est_affectable e =
  match e with
    Variable _ -> true
  | Fonction (_, _, l) -> List.for_all (fun e -> match e with Variable _ -> true | _ -> false) l
  | _ -> false

let tous_affectable e =
  match e with
    Tuple (_, l) -> List.for_all est_affectable l
  | e -> est_affectable e

let exrp_l_to_string_l l = List.map (fun e -> match e with Variable x -> x | _ -> "error") l

let expr_to_def = function
    Variable x, e -> Definition_Variable x, e
  | Fonction (name, _, l), e -> Definition_fonction (name, exrp_l_to_string_l l), e
  | _, _ -> Pas_affectable, Vide

let exprs_to_def = function
    Variable x, e -> Definition [Definition_Variable x, e]
  | Fonction (name, _, l), e -> Definition [Definition_fonction (name, exrp_l_to_string_l l), e]
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
            Vide, _ -> Erreur ("membre droit incorrect", [])
          | e2, [] -> exprs_to_def (e, e2)
          | _, l -> Erreur ("caracteres restant", l)
        else
          Erreur ("membre de gauche non assignable", [])
      )
  | Vide, l -> Erreur ("Erreur de syntaxe", l)
  | e, [] -> Expression e
  | _, l -> Erreur ("caracteres restant", l)


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
