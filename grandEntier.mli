type grandentier = bool * int list

val signe : int list -> bool

val comparer : int list -> int list -> int

val additionner : int list -> int list -> bool * int list

val soustraire : int list -> int list -> bool * int list

val multiplier : int list -> int list -> bool * int list

val pgcd : int list -> int list -> bool * int list

val diviser_multiple : int list -> int list -> bool * int list

val diviser : int list -> int list -> (bool * int list) * (bool * int list)

val grandentier_depuis_texte : string -> bool * int list

val texte_depuis_grandentier : bool * int list -> string
