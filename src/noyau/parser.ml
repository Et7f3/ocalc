open Lien
open Type
open Utils
open Lexer

let grandentier_base_de_texte t(*exte*) = Textenonvalide t

let variable_de_texte t(*texte*) = Variable t

(*let rec expr_de_texte fxs(*list of function*) t(*exte*) =
  let f = expr_de_texte fxs in
  let rec b(*oucle*) = function
      (p(*redicat*), c(*onvertisseur*)) :: l when p t -> c t
    | e :: l -> b l
    | []  -> Textenonvalide t
  in b fxs
*)
let texte_de_expr = function
      Variable nom -> nom
  | Entier ga -> GrandEntier.texte_depuis_grandentier ga
  | Textenonvalide s -> s
