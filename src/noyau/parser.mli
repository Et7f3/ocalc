val ge: string -> Lien.GrandEntier.grandentier

val variable_de_entier : (string -> Type.expr) -> string -> Type.expr

val variable_de_texte : (string -> Type.expr) -> string -> Type.expr

val variable_de_addition_soustraction : (string -> Type.expr) -> string -> Type.expr

val variable_de_multiplication_division : (string -> Type.expr) -> string -> Type.expr

val texte_de_expr : ?paren: bool -> Type.expr -> string

val expr_de_texte_etend : ((string -> bool) * ((string -> Type.expr) -> string -> Type.expr)) list -> string -> Type.expr

val expr_de_texte : string -> Type.expr
