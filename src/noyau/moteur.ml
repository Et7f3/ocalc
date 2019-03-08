open Lien
open Type

let rec additionner = function
    [], _ -> failwith "Chut OCaml"
  | acc, [] -> List.rev acc
  | e :: acc, (a :: l) ->
    match e, a with
    | Entier e, Entier a ->
      let res = Entier (GrandEntier.additionner e a)
      in additionner (res :: acc, l)
    | _ -> additionner (a :: e :: acc, l)

let rec eval = function
    Operation ("+", []) -> failwith "euh il y a un problème"
  | Operation ("+", [e]) -> eval e
  | Operation ("+", e :: l) ->
    let acc = [eval e] in
    Operation ("+", additionner (acc, l))
  | a -> a
