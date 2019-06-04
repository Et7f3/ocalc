type grandq = bool * int list * int list



let additionerq (a, b, c) (d, e, f) =
  let z, elie, travail = bigint_times b f, bigint_times e c, bigint_times c f in
  let alex, gautier = additioner (a, z) (d, elie) in
  let quentin, tropdeu = diviser (false, gautier) (false, travail) in
  alex, quentin, tropdeu

let soustractionq (a, b, c) (d, e, f) =
  let z, elie, travail = bigint_times b f, bigint_times e c, bigint_times c f in
  let alex, gautier = soustraire (a, z) (d, elie) in
  let quentin, tropdeu = diviser (false, gautier) (false, travail) in
  alex, quentin, tropdeu

(**
let soustractionq (a, b, c) (d, e, f) =
  additionerq (a, b, c) (not d, e, f)
**)

let multiplierq (a, b, c) (d, e, f) =
  let z, y = bigint_times b e, bigint_times c f in
  let x, i = diviser (false, z) (false, y) in
  a != d, x, i

let diviserq (a, b, c) (d, e, f) = multiplierq (a, b, c) (d, f, e)
