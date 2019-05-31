type operateur =
    Plus
  | Moins
  | Fois
  | Division
  | Exposant
  | Definition

type token =
    Erreur
  | EOI
  | Blanc
  | Digit of string
  | Identifiant of string
  | Affectation
  | Parenthese_ouvrante
  | Parenthese_fermante
  | Separateur_unite
  | Separateur_liste
  | Souligne
  | Operateur of operateur

let extraire_blanc s l start =
  let aux i =
    if i = start then
      i, Erreur
    else
      i, Blanc
  in let rec loop i =
    if i = l then
      aux i
    else
      match s.[i] with
        ('\t' | '\n' | ' ') -> loop (i + 1)
      | _ -> aux i
  in loop start

let extraire_digit s l start =
  let aux i =
    if i = start then
      i, Erreur
    else
      i, Digit (String.sub s start (i - start))
  in let rec loop i =
    if i = l then
      aux i
    else
      match s.[i] with
        '0' .. '9' -> loop (i + 1)
      | _ -> aux i
  in loop start

let extraire_identifiant s l start =
  let aux i =
    if i = start then
      i, Erreur
    else
      i, Identifiant (String.sub s start (i - start))
  in let rec loop i =
    if i = l then
      aux i
    else
      match s.[i] with
        ('a' .. 'z' | 'A' .. 'Z' | '\'' | '_') -> loop (i + 1)
      | _ -> aux i
  in (* loop start *)
    if start = l then
      aux start
    else
      match s.[start] with
        ('a' .. 'z' | 'A' .. 'Z') -> loop (start + 1)
      | _ -> aux start

let extraire_jeton_general m lon s l start =
  if start + lon <= l then
    let rec loop i =
      if i - start = lon then
        i, true
      else if s.[i] = m.[i - start] then
        loop (i + 1)
      else
        i, false
    in loop start
  else
    start, false

let extraire_parenthese_ouvrante s l start =
  match extraire_jeton_general "(" 1 s l start with
  | i, true -> i, Parenthese_ouvrante
  | _ -> start, Erreur

let extraire_parenthese_fermante s l start =
  match extraire_jeton_general ")" 1  s l start with
  | i, true -> i, Parenthese_fermante
  | _ -> start, Erreur

let extraire_separateur_unite s l start =
  match extraire_jeton_general "," 1 s l start with
  | i, true -> i, Separateur_unite
  | _ -> start, Erreur

let extraire_separateur_liste s l start =
  match extraire_jeton_general ";" 1 s l start with
  | i, true -> i, Separateur_liste
  | _ -> start, Erreur

let extraire_souligne s l start =
  match extraire_jeton_general "_" 1 s l start with
  | i, true -> i, Souligne
  | _ -> start, Erreur

let extraire_signe_plus s l start =
  match extraire_jeton_general "+" 1  s l start with
  | i, true -> i, Operateur Plus
  | _ -> start, Erreur

let extraire_signe_moins s l start =
  match extraire_jeton_general "-" 1  s l start with
  | i, true -> i, Operateur Moins
  | _ -> start, Erreur

let extraire_signe_fois s l start =
  match extraire_jeton_general "*" 1  s l start with
  | i, true -> i, Operateur Fois
  | _ -> start, Erreur

let extraire_signe_diviser s l start =
  match extraire_jeton_general "/" 1  s l start with
  | i, true -> i, Operateur Division
  | _ -> start, Erreur

let extraire_signe_exposant s l start =
  match extraire_jeton_general "^" 1  s l start with
  | i, true -> i, Operateur Exposant
  | _ -> start, Erreur

let extraire_signe_def s l start =
  match extraire_jeton_general ":=" 2  s l start with
  | i, true -> i, Operateur Definition
  | _ -> start, Erreur

let iter = [
    extraire_blanc;
    extraire_digit;
    extraire_identifiant;
    extraire_parenthese_ouvrante;
    extraire_parenthese_fermante;
    extraire_separateur_unite;
    extraire_separateur_liste;
    extraire_souligne;
    extraire_signe_plus;
    extraire_signe_moins;
    extraire_signe_fois;
    extraire_signe_diviser;
    extraire_signe_exposant;
    extraire_signe_def;
  ]

let symbole_suivant s l start =
  if l = start then
    l, EOI
  else
      let rec loop = function
          f :: reste ->
            let i, qqch = f s l start in
            if qqch = Erreur then
              loop reste
            else
              i, qqch
        | [] -> start, Erreur
      in loop iter

let to_lexbuf s =
  let start = 0 in
  let l = String.length s in
  let rec boucle acc start =
    match symbole_suivant s l start with
      start, EOI -> List.rev acc
    | start, Erreur -> [] (* comment je gère ? *)
    | start, qqch -> boucle (qqch :: acc) start
  in boucle [] start
