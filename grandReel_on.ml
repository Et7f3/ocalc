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

  (** 1 si ga < gb sinon 0 si ga = gb sinon -1 *)
  (* 1 : gb > ga
  0 : ga = gb
  -1 : ga > gb*)
let comparer ga gb = 0

(** renvoie ga + gb *)
let additionner ga gb = zero

(** renvoie ga - gb *)
let soustraire ga gb = zero

(** renvoie ga * gb *)
let multiplier ga gb = zero

(** renvoie ga / gb *)
let diviser ga gb = zero

(** renvoie le grandreel à partir de sa représentation textuelle *)
let grandreel_depuis_texte s = zero

(** renvoie la représentation textuelle d'un grandreel *)
let texte_depuis_grandreel ga = ""
