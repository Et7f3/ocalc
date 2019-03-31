[%%shared
    open Eliom_lib
    open Eliom_content
    open Html.F
]

open Route


let noeud service texte = li [a service [txt texte] ()]

let contenu = ul ~a:[a_id "bar_menu"] [
  noeud accueil_service "Accueil";
  noeud presentation_service "Présentation";
  noeud charges_service "Cahier des Charges";
  noeud telechargement_service "Téléchargement";
  noeud contact_service "Contact";
]
