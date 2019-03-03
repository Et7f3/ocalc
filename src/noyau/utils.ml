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
  in let len = String.length t in
  let rec boucle p(*arenthèses*) c(*rochets*) a(*ccolades*) i =
    if i < len then
      match t.[i] with
        '(' -> boucle (p + 1) c a (i + 1)
      | '[' -> boucle p (c + 1) a (i + 1)
      | '{' -> boucle p c (a + 1) (i + 1)
      | ')' -> boucle (p - 1) c a (i + 1)
      | ']' -> boucle p (c - 1) a (i + 1)
      | '}' -> boucle p c (a - 1) (i + 1)
      | lettre -> p = 0 && c = 0 && a = 0 && car_dans_liste lettre || boucle p c a (i + 1)
    else
      false
  in boucle 0 0 0 0

let couper_texte  t(*exte*) l(*iste à chercher*) =
  let car_dans_liste c =
    let rec boucle = function
        []-> false
      | e :: l -> c = e || boucle l
    in boucle l
  in let len = String.length t in
  let start = ref 0 in
  let rec boucle p(*arenthèses*) c(*rochets*) a(*ccolades*) i acc =
    if i < len then
      match t.[i] with
        '(' -> boucle (p + 1) c a (i + 1) acc
      | '[' -> boucle p (c + 1) a (i + 1) acc
      | '{' -> boucle p c (a + 1) (i + 1) acc
      | ')' -> boucle (p - 1) c a (i + 1) acc
      | ']' -> boucle p (c - 1) a (i + 1) acc
      | '}' -> boucle p c (a - 1) (i + 1) acc
      | lettre -> if p = 0 && c = 0 && a = 0 && car_dans_liste lettre then
          let acc = String.sub t !start (i - !start) :: acc in
          let () = start := i + 1 in
          boucle p c a (i + 1) acc
        else
          boucle p c a (i + 1) acc
    else
      let acc = String.sub t !start (i - !start) :: acc in
      List.rev acc
  in boucle 0 0 0 0 []

let parenthese_correcte t(*exte*)=
  let len = String.length t in
  let rec boucle i acc =
    if i < len then
      match t.[i], acc with
        ('(' as o), _ | ('[' as o), _ | ('{' as o), _ -> boucle (i + 1) (o :: acc)
      | ')', '(' :: acc -> boucle (i + 1) acc
      | ']', '[' :: acc -> boucle (i + 1) acc
      | '}', '{' :: acc -> boucle (i + 1) acc
      | ')', _ :: _ | ']', _ :: _ | '}', _ :: _ | ')', [] | ']', [] | '}', [] -> false
      | _ -> boucle (i + 1) acc
    else
      acc = []
  in boucle 0 []
