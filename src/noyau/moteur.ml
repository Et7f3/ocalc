open Lien

type context =
    Nouveau_type.expr list (* history *)
  * (Nouveau_type.affe, Nouveau_type.expr) Hashtbl.t (* definitions *)

let empty_context = [], Hashtbl.create 20

let remplace_inconnu contexte =
  let open Nouveau_type in
  let rec ri = function
      Var s when Hashtbl.find_opt contexte (Def_Var s) <> None ->
        Hashtbl.find contexte (Def_Var s)
    (*| Fx (nom, n, _) when Hashtbl.find_opt s (Nouveau_type.Def_Fx (nom, n)) <> None ->
      Hashtbl.find s (Nouveau_type.Def_Fx (nom, n)) *)
    | T (n, l) -> T (n, List.map ri l)
    | Fx (nom, n, l) -> Fx (nom, n, List.map ri l)
    | Op (`Multiplication, l) -> Op (`Multiplication, List.map ri l)
    | Op (`Addition, l) -> Op (`Addition, List.map ri l)
    | Inv e -> Inv (ri e)
    | Neg e -> Neg (ri e)
    | e -> e
  in ri

let rec eval =
  let open Nouveau_type in
  function
  | Inv (Neg e) -> Neg (eval (Inv (eval e)))
  | Neg (Neg e) -> eval e
  | Inv (Inv e) -> eval e
  | (C _ | N _ | Var _ ) as e -> e
  | T (n, l) -> T (n, List.map eval l)
  | Fx ("sin", 1, [C Pi]) -> N (GrandNum.zero)
  | Fx (nom, n, l) -> Fx (nom, n, List.map eval l)
  | Op (`Addition, l) ->
    (
      match List.map eval l |> additioner [] with
        [] ->
          let () = prerr_endline "On additionne rien du tout." in
          N (GrandNum.E (GrandEntier.grandentier_depuis_texte "0"))
      | [e] -> e
      | l -> Op (`Addition, l)
    )
  | Op (`Multiplication, l) ->
    (
      match List.map eval l |> multiplier [] with
        [] ->
          let () = prerr_endline "On multiplie rien du tout." in
          N (GrandNum.E (GrandEntier.grandentier_depuis_texte "0"))
      | [e] -> e
      | l -> Op (`Multiplication, l)
    )
  | Inv (N n) -> N (GrandNum.inverser n)
  | Neg (N n) -> N (GrandNum.opposer n)
  | Inv e -> Inv (eval e)
  | Neg e -> Neg (eval e)

and additioner acc liste_expr =
  let open Nouveau_type in
  match acc, liste_expr with
    acc, Op (`Addition, l) :: l' -> additioner acc (l @ l')
  | N n1 :: acc, N n2 :: l ->
    let n = GrandNum.additioner (n1, n2) in
    additioner acc ((N n) :: l)
  | N n :: acc, e :: l -> additioner acc (e :: N n :: l)
  | acc, e :: l -> additioner (e :: acc) l
  | acc, [] -> List.rev acc

and multiplier acc liste_expr =
  let open Nouveau_type in
  match acc, liste_expr with
    acc, Op (`Multiplication, l) :: l' -> multiplier acc (l @ l')
  | N n1 :: acc, N n2 :: l ->
    let n = GrandNum.multiplier (n1, n2) in
    multiplier acc ((N n) :: l)
  | N n :: acc, e :: l -> multiplier acc (e :: N n :: l)
  | C I :: acc, C J :: l ->
    multiplier acc ((C K) :: l)
  | C J :: acc, C I :: l ->
    multiplier acc ((Neg (C K)) :: l)
  | acc, e :: l -> multiplier (e :: acc) l
  | acc, [] -> List.rev acc

let rec text_depuis_expr_liste sep l =
  "(" ^ String.concat sep (List.map texte_depuis_expr l) ^ ")"

and texte_depuis_expr =
  let open Nouveau_type in
  let open Lien in
  function
    N n -> GrandNum.texte_depuis_num n
  | C Pi -> "pi"
  | C I -> "i"
  | C J -> "j"
  | C K -> "k"
  | Var s -> s
  | T (_, l) -> (text_depuis_expr_liste ";" l)
  | Fx (nom, _, l) -> nom ^ (text_depuis_expr_liste ";" l)
  | Op (`Multiplication, l) -> text_depuis_expr_liste "*" l
  | Op (`Addition, l) -> text_depuis_expr_liste "+" l
  | Inv e -> "/" ^ texte_depuis_expr e
  | Neg e -> "-" ^ texte_depuis_expr e

let evaluate_with_history s context =
  let history, def = context in
  let open Nouveau_parser in
  match parse s with
    Erreur (s, _ (* TODO: convert l *)) -> s, context
  | Expression e ->
    let e = Nouveau_type.expr_depuis_expression e in
    let e = remplace_inconnu def e in
    let e = eval e in
    texte_depuis_expr e, (e :: history, def)
  | Definition l ->
    let l = List.map (function
      a, e ->
        let a = Nouveau_type.affe_depuis_affectable a
        and e = Nouveau_type.expr_depuis_expression e in
        let e = remplace_inconnu def e in
        let e = eval e in
        a, e) l
    in let () = List.iter (fun (a, e) -> Hashtbl.add def a e) l in
    "definition valide", (history, def)
