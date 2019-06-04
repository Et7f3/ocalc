open Lien

type constantes =
    Pi
    (* Quaternion constantes*)
  | I
  | J
  | K

type expr =
    N of GrandNum.num
  | C of constantes
  | Var of string
  | T of int * expr list
  | Fx of string * int * expr list
  | Op of [ `Multiplication | `Addition ] * expr list
  | Inv of expr
  | Neg of expr

type affe =
    Def_Var of string
  | Def_Fx of string * int

let rec expr_depuis_expression =
  let open Nouveau_parser in
  function
    Vide -> failwith "impossible de convertir Vide"
  | Entier s -> N (GrandNum.E (GrandEntier.grandentier_depuis_texte s))
  | Reel s -> N (GrandNum.R (GrandReel.grandreel_depuis_texte s))
  | Variable ("pi" | "pI" | "Pi" | "PI") -> C Pi
  | Variable ("i" | "I") -> C I
  | Variable ("j" | "J") -> C J
  | Variable ("k" | "K") -> C K
  | Variable s -> Var s
  | Tuple (n, l) -> T (n, List.map expr_depuis_expression l)
  | Fonction (s, n, l) -> Fx (s, n, List.map expr_depuis_expression l)
  | Operation (a, l) -> Op (a, List.map expr_depuis_expression l)
  | Inverse e -> Inv (expr_depuis_expression e)
  | Negation e -> Neg (expr_depuis_expression e)

let affe_depuis_affectable =
  let open Nouveau_parser in
  function
    Pas_affectable -> failwith "impossible de convertir Pas_affectable"
  | Definition_Variable s -> Def_Var s
  | Definition_fonction (f, l) -> Def_Fx (f, List.length l)
