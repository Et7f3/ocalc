type num =
    E of GrandEntier_on.grandentier
  | R of GrandReel_on.grandreel
  | Q of GrandRationnel_on.grandrationnel
val zero : num
val unit : num
val powerabs : int -> int list
val up : 'a * int list * int -> 'a * int list * int list
val q_depuis_r : GrandReel_on.grandreel -> GrandRationnel_on.grandrationnel
val multiplier : num * num -> num
val soustraire : num * num -> num
val additioner : num * num -> num
val diviser : num * num -> num
val opposer : num -> num
val inverser : num -> num
val texte_depuis_num : num -> string
val est_negatif : num -> bool
