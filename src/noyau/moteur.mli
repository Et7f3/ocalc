val eval : Type.expr -> Type.expr

type context = Type.expr list

val empty_context: context
val evaluate_with_history: string -> context -> string * context
