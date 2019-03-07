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

  in match boucle premier l with
    [] ->  failwith "chut OCaml"
  | [e] -> e (* l'addition d'un terme est le terme lui même *)
  | l -> Operation ("+", l)

let parse =
  let parse = (est_addition_soustraction, variable_de_addition_soustraction) :: [] in
  let parse = (est_variable, variable_de_texte) :: parse in
  let parse = (est_entier10, variable_de_entier) :: parse in
  parse

let rec expr_de_texte_etend fxs(*list of function*) t(*exte*) =
  let f = expr_de_texte_etend fxs in
  let rec b(*oucle*) = function
      (p(*redicat*), c(*onvertisseur*)) :: l when p t -> c f t
    | e :: l -> b l
    | []  -> Textenonvalide t
  in b fxs

let expr_de_texte = expr_de_texte_etend parse

let rec texte_de_expr ?paren = function
    Variable nom -> nom
  | Entier ga -> GrandEntier.texte_depuis_grandentier ga
  | Neg e -> "(-" ^ texte_de_expr e ^ ")"
  | Textenonvalide s -> s
  | Operation (op, []) -> failwith "On a une operation sans operande"
  | Operation (op, e :: []) -> texte_de_expr e
  | Operation (op, e :: l) ->
    let s =
      texte_de_expr e ^ op ^ (texte_de_expr ?paren:(Some false) (Operation (op, l))) in
    match paren with
      None | Some true -> "(" ^ s ^ ")"
    | Some false -> s
