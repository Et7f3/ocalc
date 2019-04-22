open Lien
open Type

let rec additionner = function
    [], [] -> failwith "Chut OCaml: additioner rien du tout ?"
  | [], e :: l -> additionner ([eval e], l)
  | acc, [] -> List.rev acc
  | e :: acc, (a :: l) ->
    match eval e, eval a with
    | Entier a, Variable e -> additionner (Variable e :: acc, Entier a :: l)
    | Entier e, Entier a ->
      let res = Entier (GrandEntier.additionner e a) in
      additionner (res :: acc, l)
    | e, a -> additionner (a :: e :: acc, l)

and multiplier = function
    [], [] -> failwith "Chut OCaml: multiplier rien du tout ?"
  | [], e :: l -> multiplier ([eval e], l)
  | acc, [] -> List.rev acc
  | e :: acc, Inv a :: l when (match e with Inv _ -> false | _ -> true) ->
    multiplier (acc, Inv a :: e :: l)
  | e :: acc, (a :: l) ->
    match eval e, eval a with
    | Entier a, Variable e -> multiplier (Variable e :: acc, Entier a :: l)
    | Entier e, Entier a ->
      let res = Entier (GrandEntier.multiplier e a)
      in multiplier (res :: acc, l)
    | Entier e, Inv (Entier a) | Inv (Entier a), Entier e ->
      (match GrandEntier.diviser e a with
          e, x when x = GrandEntier.unit -> multiplier ((Entier e) :: acc, l)
        | x, a when x = GrandEntier.unit -> multiplier (acc, Inv (Entier a) :: l)
        | e, a -> multiplier ((Entier e) :: Inv (Entier a) :: acc, l))
    | Inv (Entier e), Inv (Entier a) ->
      let res = GrandEntier.multiplier e a in
      multiplier (acc, Inv (Entier res) :: l)
    | e, a -> multiplier (a :: e :: acc, l)

and eval = function
    Operation ("+", []) -> failwith "euh il y a un problème"
  | Operation ("+", [e]) -> eval e
  | Operation ("+", e :: l) ->
    let acc = [eval e] in
    (match additionner (acc, l) with
       [e] -> e
     | l -> Operation ("+", l))
  | Operation ("*", []) -> failwith "euh il y a un problème"
  | Operation ("*", [e]) -> eval e
  | Operation ("*", e :: l) ->
    let acc = [eval e] in
    (match multiplier (acc, l) with
       [e] -> e
     | l -> Operation ("*", l))
  | Neg (Neg e) -> eval e
  | a -> a

type context = expr list

let empty_context = []

let evaluate_with_history str context =
    let e = eval (Parser.expr_de_texte str) in
    Parser.texte_de_expr e, e :: context
