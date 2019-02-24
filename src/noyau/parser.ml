open Type
open Utils
open Lexer

let grandentier_de_texte t(*exte*) =
  let l(*ongueur*) = String.length t in
  let s(*ign*), t, l =
    match t.[0] with
      '-' -> -1, String.sub t 1 (l - 1), l - 1
    | '+' -> 1, String.sub t 1 (l - 1), l - 1
    | _ -> 1, t, l
  in let rec b(*oucle*) i acc =
       if i = -1 then
         List.rev acc
       else
         b (i - 1) ((int_of_char t.[i] - 48) :: acc)
  in Entier (s, b (l - 1) [])

let grandentier_base_de_texte t(*exte*) = Textenonvalide t

let variable_de_texte t(*texte*) = Variable t

let rec expr_de_texte fxs(*list of function*) t(*exte*) =
  let f = expr_de_texte fxs in
  let rec b(*oucle*) = function
      (p(*redicat*), c(*onvertisseur*)) :: l when p t -> c t
    | e :: l -> b l
    | []  -> Textenonvalide t
  in b fxs
