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
    Inv e ->
    (
      match eval e with
        Inv e -> e
      | N (n) -> N (GrandNum.inverser n)
      | Neg e -> Neg (Inv e)
      | e -> Inv e
    )
  | Neg e ->
    (
      match eval e with
        Neg e -> e
      | N (n) -> N (GrandNum.opposer n)
      | e -> Neg e
    )
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
  | N e :: _, _ when e = GrandNum.zero -> [N (GrandNum.zero)]
  | _, N e :: _ when e = GrandNum.zero -> [N (GrandNum.zero)]
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

let rec text_de_multiplication l =
  let open Nouveau_type in
  let rec boucle acc = function
      [] -> acc
    | Op (`Addition, l) :: k -> boucle (acc ^ "(" ^ texte_de_addition l ^ ")") k
    | Inv (Op (_, _) as e) :: l ->
      let e = texte_depuis_expr e in
      boucle (acc ^ " / " ^ e) l
    | Inv e :: l -> boucle (acc ^ " / " ^ (texte_depuis_expr e)) l
    | e :: l -> boucle (acc ^ " * " ^ (texte_depuis_expr e)) l
  in match l with
    [] -> boucle "" l
  | Op (`Addition, l) :: k -> boucle ("(" ^ texte_de_addition l ^ ")") k
  | Inv (Op (_, _) as e) :: l ->
    let e = texte_depuis_expr e in
    boucle ("1 / " ^ e) l
  | Inv e :: l -> boucle ("1 / " ^ (texte_depuis_expr e)) l
  | e :: l -> boucle (texte_depuis_expr e) l

and texte_de_addition l =
  let rec boucle acc = function
      [] -> acc
    | e :: l ->
      let e = texte_depuis_expr e in
      if e.[0] = '-' then
        boucle (acc ^ e) l
      else
        boucle (acc ^ " + " ^ e) l
  in match l with
    [] -> boucle "" l
  | e :: l -> boucle (texte_depuis_expr e) l

and text_depuis_expr_liste sep l =
  "(" ^ String.concat sep (List.map texte_depuis_expr l) ^ ")"

and texte_depuis_expr =
  let open Nouveau_type in
  let open Lien in
  function
    N n ->
      let e = GrandNum.texte_depuis_num n in
      if GrandNum.est_negatif n then
        "(" ^ e ^ ")"
      else
        e
  | C Pi -> "pi"
  | C I -> "i"
  | C J -> "j"
  | C K -> "k"
  | Var s -> s
  | T (_, l) -> (text_depuis_expr_liste ";" l)
  | Fx (nom, _, l) -> nom ^ (text_depuis_expr_liste ";" l)
  | Op (`Multiplication, l) -> text_de_multiplication l
  | Op (`Addition, l) -> texte_de_addition l
  | Inv e -> " / " ^ texte_depuis_expr e
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
    I18n.definition_valide (), (history, def)


module Expr = struct
    open Nouveau_type
    type t = expr

    let zero = N (GrandNum.zero)
    let unit = N (GrandNum.unit)
    let symb s = Var s
    let neg e = eval (Neg e)
    let est_zero a = a = N (GrandNum.zero)
    let depuis_texte s =
      match Nouveau_parser.parse s with
        Expression e ->
          let e = Nouveau_type.expr_depuis_expression e in
          eval e
      | Erreur (s, _ (* TODO: convert l *)) ->
        failwith (s)
      | _ -> failwith (I18n.definition_non_autorise ())
    let vers_texte = texte_depuis_expr
    let additioner a b = eval (Op (`Addition, [a; b]))
    let soustraire a b = eval (Op (`Addition, [a; Neg b]))
    let diviser a b = eval (Op (`Multiplication, [a; Inv b]))
    let multiplier a b = eval (Op (`Multiplication, [a; b]))
    let print e = print_endline (texte_depuis_expr e)
end

module Expr_matrix = Modules.Matrix.Generic_matrix(Expr)
