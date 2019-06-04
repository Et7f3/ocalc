type context =
    Nouveau_type.expr list (* history *)
  * (Nouveau_type.affe, Nouveau_type.expr) Hashtbl.t (* definitions *)

let empty_context = [], Hashtbl.create 20

let rec text_depuis_expr_liste sep l =
  String.concat sep (List.map texte_depuis_expr l)

and texte_depuis_expr =
  let open Nouveau_type in
  let open Lien in
  function
    E e -> GrandEntier.texte_depuis_grandentier e
  | R r -> GrandReel.texte_depuis_grandreel r
  | C Pi -> "pi"
  | C I -> "i"
  | C J -> "j"
  | C K -> "k"
  | Var s -> s
  | T (_, l) -> "(" ^ (text_depuis_expr_liste ";" l) ^ ")"
  | Fx (nom, _, l) -> nom ^ "(" ^ (text_depuis_expr_liste ";" l) ^ ")"
  | Op (`Multiplication, l) -> text_depuis_expr_liste "*" l
  | Op (`Addition, l) -> text_depuis_expr_liste "+" l
  | Inv e -> "/" ^ texte_depuis_expr e
  | Neg e -> "-" ^ texte_depuis_expr e

let evaluate_with_history s (history, def as context) =
  let open Nouveau_parser in
  match parse s with
    Erreur (s, _ (* TODO: convert l *)) -> s, context
  | Expression e ->
    let e = Nouveau_type.expr_depuis_expression e in
    texte_depuis_expr e, (e :: history, def)
  | Definition l -> "definition valide", context (* TODO: evaluate definitions vefore storing them *)
