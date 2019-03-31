[%%shared
    open Eliom_lib
    open Eliom_content
    open Html.F
]

module Ocalc_app =
  Eliom_registration.App (
    struct
      let application_name = "ocalc"
      let global_data_path = None
    end)

open Route

let () =
  List.iter (fun (service, page) ->
  Ocalc_app.register
    ~service
    page
) [
  accueil_service, Accueil.page;
  presentation_service, Presentation.page;
  charges_service, Charges.page;
  telechargement_service, Telechargement.page;
  contact_service, Contact.page;
]

let () =
  Ocalc_app.register
    ~service:utilisateur_service
    Utilisateur.page
