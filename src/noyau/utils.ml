let print_bool b =
  print_endline (
    if b then
      "true"
    else
      "false"
  )

let contient_texte t(*exte*) l(*iste à chercher*) =
  let car_dans_liste c =
    let rec boucle = function
        []-> false
      | e :: l -> c = e || boucle l
    in boucle l
  in
  let len = String.length t in
  let rec boucle p(*arenthèses*) c(*rochets*) a(*ccolades*) i =
    if i < len then
      match t.[i] with
        '(' -> boucle (p + 1) c a (i + 1)
      | '[' -> boucle p (c + 1) a (i + 1)
      | '{' -> boucle p c (a + 1) (i + 1)
      | ')' -> boucle (p - 1) c a (i + 1)
      | ']' -> boucle p (c - 1) a (i + 1)
      | '}' -> boucle p c (a - 1) (i + 1)
      | lettre -> car_dans_liste lettre || boucle p c a (i + 1)
    else
      false
in boucle 0 0 0 0

let couper_texte  t(*exte*) l(*iste à chercher*) = []
