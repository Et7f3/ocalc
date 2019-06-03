open Lien

type expr =
    E of GrandEntier.grandentier
  | R of GrandReel.grandreel
  | Var of string
  | T of int * expr list
  | Fx of string * int * expr list
  | Op of [ `Multiplication | `Addition ] * expr list
  | Inv of expr
  | Neg of expr

let rec expr_depuis_expression =
  let open Nouveau_parser in
  function
    Vide -> failwith "impossible de convertir Vide"
  | Entier s -> E (GrandEntier.grandentier_depuis_texte s)
  | Reel s -> R (GrandReel.grandreel_depuis_texte s)
  | Variable s -> Var s
  | Tuple (n, l) -> T (n, List.map expr_depuis_expression l)
  | Fonction (s, n, l) -> Fx (s, n, List.map expr_depuis_expression l)
  | Operation (a, l) -> Op (a, List.map expr_depuis_expression l)
  | Inverse e -> Inv (expr_depuis_expression e)
  | Negation e -> Neg (expr_depuis_expression e)
