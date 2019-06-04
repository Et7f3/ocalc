type context

val empty_context: context
val evaluate_with_history: string -> context -> string * context
