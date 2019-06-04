type grandentier = int
(** grandentier est un tuple [(signe négatif, \[unité, dizaine, centaine, ...\])]
    [-123] correspond à [(true, \[3; 2; 1\])] *)

let zero = 0

let unit = 1

let est_negatif x = x < 0
(** renvoie [grandentier < 0] *)

let neg = (~-)
(** renvoie l'oposé *)

let comparer ga gb =
  if ga < gb then
    1
  else if ga = gb then
    0
  else
    -1
(** 1 si ga < gb sinon 0 si ga = gb sinon -1 *)

let additioner ga gb = ga + gb
(** renvoie ga + gb *)

let soustraire ga gb = ga - gb
(** renvoie ga - gb *)

let multiplier ga gb = ga * gb
(** renvoie ga * gb *)

let rec pgcd ga gb =
  if ga = 0 then
    gb
  else
    pgcd (gb mod ga) ga
(** renvoie pgcd(ga, gb) *)

let diviser_multiple ga gb = ga / gb
(** renvoie ga / gb où ga est multiple de gb *)

let diviser ga gb =
  let pgcd = pgcd ga gb in
  ga / pgcd, gb / pgcd
(** renvoie (nominateur, dénominateur) de la fraction ga / gb *)

let grandentier_depuis_texte sa = int_of_string sa
(** renvoie le grandentier à partir de sa représentation textuelle *)

let texte_depuis_grandentier ga = string_of_int ga
(** renvoie la représentation textuelle d'un grandentier *)
