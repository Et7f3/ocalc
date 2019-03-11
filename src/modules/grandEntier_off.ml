type grandentier = int
(** grandentier est un tuple [(signe négatif, \[unité, dizaine, centaine, ...\])]

    [-123] correspond à [(true, \[3; 2; 1\])] *)

let est_negatif x = x < 0
(** renvoie [grandentier < 0] *)

let comparer ga gb =
  if ga < gb then
    1
  else if ga = gb then
    0
  else
    -1
(** 1 si ga < gb sinon 0 si ga = gb sinon -1 *)

let additionner ga gb = ga + gb
(** renvoie ga + gb *)

let soustraire ga gb = ga - gb
(** renvoie ga - gb *)

let multiplier ga gb = ga * gb
(** renvoie ga * gb *)

let pgcd ga gb = 0
(** renvoie pgcd(ga, gb) *)

let diviser_multiple ga gb = 0
(** renvoie ga / gb où ga est multiple de gb *)

let diviser ga gb = 0, 0
(** renvoie (nominateur, dénominateur) de la fraction ga / gb *)

let grandentier_depuis_texte sa = int_of_string sa
(** renvoie le grandentier à partir de sa représentation textuelle *)

let texte_depuis_grandentier ga = string_of_int ga
(** renvoie la représentation textuelle d'un grandentier *)
