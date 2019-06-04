type grandreel = bool * int list * int
val zero : grandreel
val unit : grandreel
val est_negatif : grandreel -> bool
val neg : grandreel -> grandreel
val comparer : grandreel -> grandreel -> int
val additioner : grandreel -> grandreel -> grandreel
val soustraire : grandreel -> grandreel -> grandreel
val multiplier : grandreel -> grandreel -> grandreel
val diviser : grandreel -> grandreel -> grandreel * grandreel
val grandreel_depuis_texte : string -> grandreel
val texte_depuis_grandreel : grandreel -> string
