val variable_de_entier : (string -> Type.expr) -> string -> Type.expr

val variable_de_texte : (string -> Type.expr) -> string -> Type.expr

val texte_de_expr : ?paren: bool -> Type.expr -> string

val expr_de_texte : ((string -> bool) * ((string -> Type.expr) -> string -> Type.expr)) list -> string -> Type.expr
