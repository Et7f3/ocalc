type grandentier = bool * int list
val zero : grandentier
val unit : grandentier
val est_negatif : grandentier -> bool
val neg : grandentier -> grandentier
val comparer : grandentier -> grandentier -> int
val additioner : grandentier -> grandentier -> grandentier
val soustraire : grandentier -> grandentier -> grandentier
val multiplier : grandentier -> grandentier -> grandentier
val pgcd : grandentier -> grandentier -> grandentier
val diviser_multiple : grandentier -> grandentier -> grandentier
val diviser : grandentier -> grandentier -> grandentier * grandentier
val grandentier_depuis_texte : string -> grandentier
val texte_depuis_grandentier : grandentier -> string
