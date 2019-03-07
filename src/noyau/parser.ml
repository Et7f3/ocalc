open Lien
open Type
open Utils
open Lexer

(** only for test purpose *)
let ge = (Obj.magic (false, []) : Lien.GrandEntier.grandentier)
(** only for test purpose *)

let variable_de_entier _ t(*exte*) = Entier (GrandEntier.grandentier_depuis_texte t)

let variable_de_texte _ t(*texte*) = Variable t

let variable_de_addition_soustraction compile t(*texte*) =
  let l = couper_texte t ['+'; '-'] in
  let premier_el = function
      [] | [_] | [_; _] -> failwith "chut OCaml"
    | "" :: "-" :: e :: liste -> [Neg (compile e)], liste
    | "" :: "+" :: e :: liste -> [compile e], liste
    | e :: liste -> [compile e], liste
  in
  let (premier, l) = premier_el l in
  let rec boucle acc = function
      [_] ->  failwith "chut OCaml"
    | [] -> List.rev acc
    | "+" :: e :: liste -> boucle (compile e :: acc) liste
    | "-" :: e :: liste -> boucle (Neg (compile e) :: acc) liste
    |_-> failwith "error"

  in Operation ("+", boucle premier l)
let rec expr_de_texte fxs(*list of function*) t(*exte*) =
  let f = expr_de_texte fxs in
  let rec b(*oucle*) = function
      (p(*redicat*), c(*onvertisseur*)) :: l when p t -> c f t
    | e :: l -> b l
    | []  -> Textenonvalide t
  in b fxs

let rec texte_de_expr ?paren = function
    Variable nom -> nom
  | Entier ga -> GrandEntier.texte_depuis_grandentier ga
  | Neg e -> "-" ^ texte_de_expr e
  | Textenonvalide s -> s
  | Operation (op, []) -> failwith "une operation sain operateur"
  | Operation (op, e :: []) -> texte_de_expr e
  | Operation (op, e :: l) ->
    let s =
      texte_de_expr e ^ op ^ (texte_de_expr ?paren:(Some false) (Operation (op, l))) in
    match paren with
      None | Some true -> "(" ^ s ^ ")"
    | Some false -> s
