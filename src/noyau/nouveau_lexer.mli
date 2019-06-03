type operateur = Plus | Moins | Fois | Division | Exposant | Definition
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
val extraire_blanc : string -> int -> int -> int * token
val extraire_digit : string -> int -> int -> int * token
val extraire_identifiant : string -> int -> int -> int * token
val extraire_jeton_general : string -> int -> string -> int -> int -> int * bool
val extraire_parenthese_ouvrante : string -> int -> int -> int * token
val extraire_parenthese_fermante : string -> int -> int -> int * token
val extraire_separateur_unite : string -> int -> int -> int * token
val extraire_separateur_liste : string -> int -> int -> int * token
val extraire_souligne : string -> int -> int -> int * token
val extraire_signe_plus : string -> int -> int -> int * token
val extraire_signe_moins : string -> int -> int -> int * token
val extraire_signe_fois : string -> int -> int -> int * token
val extraire_signe_diviser : string -> int -> int -> int * token
val extraire_signe_exposant : string -> int -> int -> int * token
val extraire_signe_def : string -> int -> int -> int * token
val iter : (string -> int -> int -> int * token) list
val symbole_suivant : string -> int -> int -> int * token
val to_lexbuf : string -> token list
