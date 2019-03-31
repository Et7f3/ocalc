[%%shared
    open Eliom_lib
    open Eliom_content
    open Html.F
]

let accueil_service =
  Eliom_service.create
    ~path:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

let presentation_service =
  Eliom_service.create
    ~path:(Eliom_service.Path ["presentation"])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()


let charges_service =
  Eliom_service.create
  ~path:(Eliom_service.Path ["charges"])
  ~meth:(Eliom_service.Get Eliom_parameter.unit)
  ()


let telechargement_service =
  Eliom_service.create
  ~path:(Eliom_service.Path ["telechargement"])
  ~meth:(Eliom_service.Get Eliom_parameter.unit)
  ()


let contact_service =
  Eliom_service.create
  ~path:(Eliom_service.Path ["contact"])
  ~meth:(Eliom_service.Get Eliom_parameter.unit)
  ()

let utilisateur_service =
  Eliom_service.create
    ~path:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get (Eliom_parameter.(suffix (string "prenom" ** string "nom"))))
    ()
