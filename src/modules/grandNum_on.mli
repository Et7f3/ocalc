type num =
    E of GrandEntier_on.grandentier
  | R of GrandReel_on.grandreel
  | Q of GrandRationnel_on.grandrationnel
val powerabs : int -> int list = <fun>
val up : 'a * int list * int -> 'a * int list * int list = <fun>
val truc : 'a * int list * int -> 'a * int list * int list = <fun>
val multiplier : num * num -> num = <fun>
val soustraire : num * num -> num = <fun>
val additioner : num * num -> num = <fun>
val diviser : num * num -> num = <fun>
val opposer : num -> num = <fun>
val inverser : num -> num = <fun>
