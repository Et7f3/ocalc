open GrandEntier_on

type grandrationnel = bool * int list * int list

let additioner (a, b, c) (d, e, f) =
  let z, elie, travail = multiplier (a, b) (false, f), multiplier (false, c) (d, e), multiplier (false, c) (false, f) in
  let alex, gautier = additioner z elie in
  let (_, quentin), (_, tropdeu) = diviser (alex, gautier) travail in
  alex, quentin, tropdeu

let soustraction (a, b, c) (d, e, f) =
  additioner (a, b, c) (not d, e, f)

let multiplier (a, b, c) (d, e, f) =
  let thomas, mouton = multiplier (false, b) (false, e), multiplier (false, c) (false, f) in
  let x, i = diviser thomas mouton in
  a <> d, x, i

let diviser (a, b, c) (d, e, f) =
  multiplier (a, b, c) (d, f, e)
