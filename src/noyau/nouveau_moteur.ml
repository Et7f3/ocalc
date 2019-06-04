type context =
    Nouveau_type.expr list (* history *)
  * (Nouveau_parser.affectable, Nouveau_type.expr) Hashtbl.t (* definitions *)

let empty_context = [], Hashtbl.create 20

let evaluate_with_history s (history, def as context) =
  let open Nouveau_parser in
  match parse s with
    Erreur (s, l) -> s, context
  | Expression e -> "expression valide", (Nouveau_type.expr_depuis_expression e :: history, def)
  | Definition l -> "definition valide", context (* TODO: evaluate definitions vefore storing them *)
