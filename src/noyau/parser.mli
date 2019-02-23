val grandentier_de_texte : string -> Type.expr
val grandentier_base_de_texte : string -> Type.expr
val variable_de_texte : string -> Type.expr
val expr_de_texte :
  ((string -> bool) * (string -> Type.expr)) list -> string -> Type.expr
