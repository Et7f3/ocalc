type context

val empty_context: context
val evaluate_with_history: string -> context -> string * context



module Expr :
sig
  type t = Nouveau_type.expr
  val zero : t
  val unit : t
  val symb : string -> t
  val neg : t -> t
  val est_zero : t -> bool
  val depuis_texte : string -> t
  val vers_texte : t -> string
  val additioner : t -> t -> t
  val soustraire : t -> t -> t
  val diviser : t -> t -> t
  val multiplier : t -> t -> t
  val print : t -> unit
end

module Expr_matrix :
sig
  type t = Expr.t array array
  type solution_equation =
      Erreur of string
    | Solution_systeme of (string * Expr.t) list
  val solveur : string array array -> int -> int -> string array -> solution_equation
end
