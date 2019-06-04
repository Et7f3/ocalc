open GrandEntier_on

type grandrationnel = bool * int list * int list

(*
let additionerq (a, b, c) (d, e, f) =
  let z, elie, travail = bigint_times b f, bigint_times e c, bigint_times c f in
  let alex, gautier = additioner (a, z) (d, elie) in
  let quentin, tropdeu = diviser (false, gautier) (false, travail) in
  alex, quentin, tropdeu
*)

let additioner (a, b, c) (d, e, f) =
  let z, elie, travail = multiplier (a, b) (false, f), multiplier (false, c) (d, e), multiplier (false, c) (false, f) in
  let alex, gautier = additioner z elie in
  let (_, quentin), (_, tropdeu) = diviser (alex, gautier) travail in
  alex, quentin, tropdeu

let soustraire (a, b, c) (d, e, f) =
  additioner (a, b, c) (not d, e, f)

let multiplier (a, b, c) (d, e, f) =
  let thomas, mouton = multiplier (false, b) (false, e), multiplier (false, c) (false, f) in
  let (_,x), (_,i) = diviser thomas mouton in
  a <> d, x, i

let diviser (a, b, c) (d, e, f) =
  multiplier (a, b, c) (d, f, e)
