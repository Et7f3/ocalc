open Thread
open Unix

type _ valeur = Valeur: string * string -> string valeur
            | Fichier: string * Unix.file_descr -> Unix.file_descr valeur
(** type utile pour analyser le corp de la requête.
Valeur contient le couple (clef, valeur)
Fichier contient le couple (nom du fichier, descripteur de fichier) *)

type proto = HTTP (* | HTTPS | HTTP2 | Websocket *)
(** les différents type de prototypes
les types en commentaires seront les bonus *)

let instances = ref []
(** instances des différents thread crée pour chaque serveur *)

let serveur_en_ecoute () = !instances <> []
(** le serveur est-il lancé *)

let lancer_serveur gestionnaire adress port max_connections = ()
(** démarre un serveur qui écoute sur [port] de n'importe quel adresse.
Démarre un fil d'execution pour chaque connexion et passe le canal créée en paramètre au gestionnaire.
valeurs par défaut:
  adress: toutes "0.0.0.0"
  port: 80
  max_connections: 42
 *)

let arreter_tout_serveurs () = ()
(** ferme proprement toutes les connexions de tout les serveurs *)

let lire_info_http con = ("GET", "/", "HTTP/1.1", [])
(** renvoie les entêtes de la requête http venant du canal [con] 
format de réponse (méthode, chemin, version http, entêtes) *)

let lire_corps_http con entetes = []
(** renvoie les variables contenu dans la requête venant du canal [con] avec les attribut entetes *)

let gestionnaire_http_serveur con = ()
(** doit répondre à une requête http *)

let lancer_serveur_type = function
    HTTP -> lancer_serveur gestionnaire_http_serveur
(** lance un serveur en fonction du type de protocole *)

let gestionnaire_http_client con gestionnaire = ()
(** recois une réponse http et transmet le couple (version http, code, text, entêtes, corps) à gestionnaire *)

let lancer_requete_http methode addr port entetes donnees = ()
(** initie la connexion via la methode methode à addr et transmet les entêtes et donnees *)
