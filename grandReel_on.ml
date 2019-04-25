(** grandentier est un tuple [(signe négatif, \[unité, dizaine, centaine, ...\], exposant base 10)]
[-1,23] correspond à [(true, \[3; 2; 1\], -2)] *)
type grandreel = bool * int list * int

(** zero de grandreel *)
let zero = false, [], 0

(** unité de grandreel *)
let unit = false, [1], 0

(** renvoie [grandreel < 0] *)
let est_negatif (s, _, _) = s

(** renvoie l'oposé *)
let neg = function
    (_, [], _) -> false, [], 0
  | (s, m, e) -> not s, m, e


(*
Fonction Auxiliaire qui va mettre les deux valeurs sur la meme puissance
On choisira toujours la plus grande
*)
let reunir_puissance ga gb =
  let rec r_p ga gb =
    let (sa, va, a) = ga
    and (sb, vb, b) = gb in
    if a = b then
      ga, gb
    else if a < b then
      r_p (sa, va, a) (sb, 0 :: vb, b - 1)
    else
      r_p (sa, 0 :: va, a - 1) (sb, vb, b)
  in r_p ga gb

  (** 1 si ga < gb sinon 0 si ga = gb sinon -1 *)
  (* 1 : gb > ga
  0 : ga = gb
  -1 : ga > gb*)

let comparer_gr ga gb =
  let (a, b, c), (d, e, _) = reunir_puissance ga gb in
  comparer (a,b) (d,e)

(** renvoie ga + gb *)
let additionner ga gb =
  let (a, b, c), (d, e, _) = reunir_puissance ga gb in
  let a, b = additionner (a,b) (d,e) in
  a, b, c

(** renvoie ga - gb *)
let soustraire ga gb =
  let (a, b, c), (d, e, _) = reunir_puissance ga gb in
  let a, b = soustraire (a,b) (d,e) in
  a, b, c

(** renvoie ga * gb *)
let multiplier (a, b, c) (d, e, f) =
  let a, b = multiplier (a, b) (d, e) in
  a, b, c + f

(** renvoie ga / gb *)
let diviser (a, b, c) (d, e, f) =
  let () =
    if e = [] then (* / 0 *)
      failwith "Nique ta gentil maman"
  in let signe = b <> [] (* 0 / a -> + *) && a <> d in
  let b = [] in
  signe, b, c - f

(** renvoie le grandreel à partir de sa représentation textuelle *)
let grandreel_depuis_texte s = zero

(** renvoie la représentation textuelle d'un grandreel *)
let texte_depuis_grandreel ga = ""
