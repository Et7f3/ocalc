val variable_de_entier : string -> Type.expr

val variable_de_texte : string -> Type.expr

val texte_de_expr : Type.expr -> string

val expr_de_texte : ((string -> bool) * (string -> Type.expr)) list -> string -> Type.expr
