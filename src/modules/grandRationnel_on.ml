open GrandEntier_on

type grandrationnel = bool * int list * int list

let additioner (a, b, c) (d, e, f) =
  let z = multiplier (a, b) (false, f)
  and elie = multiplier (false, c) (d, e)
  and travail = multiplier (false, c) (false, f) in
  let alex, gautier = additioner z elie in
  let (_, quentin), (_, tropdeu) = diviser (alex, gautier) travail in
  alex, quentin, tropdeu

let soustraire (a, b, c) (d, e, f) =
  additioner (a, b, c) (not d, e, f)

let multiplier (a, b, c) (d, e, f) =
  let thomas = multiplier (false, b) (false, e)
  and mouton = multiplier (false, c) (false, f) in
  let (_,x), (_,i) = diviser thomas mouton in
  a <> d, x, i

let diviser (a, b, c) (d, e, f) =
  multiplier (a, b, c) (d, f, e)
