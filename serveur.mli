type _ valeur =
    Valeur : string * string -> string valeur
  | Fichier : string * Unix.file_descr -> Unix.file_descr valeur

type proto = HTTP

val instances : Thread.t list ref

val serveur_en_ecoute : unit -> bool

val lancer_serveur : ('a -> unit) -> string -> int -> int -> unit

val arreter_tout_serveurs : unit -> unit

val lire_info_http : Unix.file_descr -> string * string * string * string valeur list

val lire_corps_http : Unix.file_descr -> string valeur list -> 'c valeur list

val gestionnaire_http_serveur : Unix.file_descr -> unit

val lancer_serveur_type : proto -> string -> int -> int -> unit

val gestionnaire_http_client : Unix.file_descr -> (string * int * string * string valeur list * string -> unit) -> unit

val lancer_requete_http : string -> string -> int -> string valeur list -> 'a valeur list -> unit
