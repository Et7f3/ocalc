open Lien
open Type

let rec additionner = function
    [], _ -> failwith "Chut OCaml"
  | acc, [] -> List.rev acc
  | e :: acc, (a :: l) ->
    match eval e, eval a with
    | Entier a, Variable e -> additionner (Variable e :: acc, Entier a :: l)
    | Entier e, Entier a ->
      let res = Entier (GrandEntier.additionner e a)
      in additionner (res :: acc, l)
    | e, a -> additionner (a :: e :: acc, l)

and multiplier = function
    [], _ -> failwith "Chut OCaml"
  | acc, [] -> List.rev acc
  | e :: acc, (a :: l) ->
    match eval e, eval a with
    | Entier a, Variable e -> multiplier (Variable e :: acc, Entier a :: l)
    | Entier e, Entier a ->
      let res = Entier (GrandEntier.multiplier e a)
      in multiplier (res :: acc, l)
    | Entier e, Inv (Entier a) | Inv (Entier a), Entier e ->
      (*let (ga, gb) = GrandEntier.diviser e a in
      let res = Entier q in
      multiplier (res :: acc, l)*)
      (match GrandEntier.diviser e a with
          e, x when x = GrandEntier.unit -> multiplier ((Entier e) :: acc, l)
        | e, a -> multiplier ((Entier e) :: Inv (Entier a) :: acc, l))
    | e, a -> multiplier (a :: e :: acc, l)

and eval = function
    Operation ("+", []) -> failwith "euh il y a un problème"
  | Operation ("+", [e]) -> eval e
  | Operation ("+", e :: l) ->
    let acc = [eval e] in
    (match additionner (acc, l) with
       [e] -> e
     |l -> Operation ("+", l))
  | Operation ("*", []) -> failwith "euh il y a un problème"
  | Operation ("*", [e]) -> eval e
  | Operation ("*", e :: l) ->
    let acc = [eval e] in
    (match multiplier (acc, l) with
       [e] -> e
     |l -> Operation ("*", l))
  | Neg (Neg e) -> eval e
  | a -> a
