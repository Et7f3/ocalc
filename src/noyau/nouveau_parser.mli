type expression =
    Vide
  | Entier of string
  | Reel of string
  | Variable of string
  | Tuple of int * expression list
  | Fonction of string * int * expression list
  | Operation of [ `Addition | `Multiplication ] * expression list
  | Inverse of expression
  | Negation of expression
type affectable =
    Pas_affectable
  | Definition_Variable of string
  | Definition_fonction of string * int * string list
type entre_valide =
    Erreur of string * Nouveau_lexer.token list
  | Expression of expression
  | Definition of (affectable * expression) list
val entier :
  Nouveau_lexer.token list -> expression * Nouveau_lexer.token list
val reel :
  Nouveau_lexer.token list -> expression * Nouveau_lexer.token list
val variable :
  Nouveau_lexer.token list -> expression * Nouveau_lexer.token list
val tuple_de :
  (Nouveau_lexer.token list -> expression * Nouveau_lexer.token list) ->
  Nouveau_lexer.token list -> expression * Nouveau_lexer.token list
val fonction :
  Nouveau_lexer.token list -> expression * Nouveau_lexer.token list
val facteur :
  Nouveau_lexer.token list -> expression * Nouveau_lexer.token list
val terme :
  Nouveau_lexer.token list -> expression * Nouveau_lexer.token list
val expression :
  Nouveau_lexer.token list -> expression * Nouveau_lexer.token list
val est_affectable : expression -> bool
val tous_affectable : expression -> bool
val exrp_l_to_string_l : expression list -> string list
val expr_to_def : expression * expression -> affectable * expression
val exprs_to_def : expression * expression -> entre_valide
val expression_principale : Nouveau_lexer.token list -> entre_valide
val filtre : Nouveau_lexer.token list -> Nouveau_lexer.token list
val parse : string -> entre_valide
