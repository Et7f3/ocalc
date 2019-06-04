type num =
    E of GrandEntier_on.grandentier
  | R of GrandReel_on.grandreel
  | Q of GrandRationnel_on.grandrationnel
val powerabs : int -> int list
val up : 'a * int list * int -> 'a * int list * int list
val truc : 'a * int list * int -> 'a * int list * int list
val multiplier : num * num -> num
val soustraire : num * num -> num
val additioner : num * num -> num
val diviser : num * num -> num
val opposer : num -> num
val inverser : num -> num
val texte_depuis_num : num -> string