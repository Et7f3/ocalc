type grandentier = bool * int list
(** grandentier est un tuple [(signe négatif, \[unité, dizaine, centaine, ...\])]

    [-123] correspond à [(true, \[3; 2; 1\])] *)

let est_negatif = function x -> false
(** renvoie [grandentier < 0] *)

let comparer ga gb = 0
(** 1 si ga < gb sinon 0 si ga = gb sinon -1 *)

let additionner ga gb = (false, [])
(** renvoie ga + gb *)

let soustraire ga gb = (false, [])
(** renvoie ga - gb *)

let multiplier ga gb = (false, [])
(** renvoie ga * gb *)

let pgcd ga gb = (false, [])
(** renvoie pgcd(ga, gb) *)

let diviser_multiple ga gb = (false, [])
(** renvoie ga / gb où ga est multiple de gb *)

let diviser ga gb = ((false, []), (false, []))
(** renvoie (nominateur, dénominateur) de la fraction ga / gb *)

let grandentier_depuis_texte sa = (false, [])
(** renvoie le grandentier à partir de sa représentation textuelle *)

let texte_depuis_grandentier ga = ""
(** renvoie la représentation textuelle d'un grandentier *)
