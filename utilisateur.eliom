[%%shared
    open Eliom_lib
    open Eliom_content
    open Html.F
]

let page (prenom, nom) () =
  Lwt.return
        (Eliom_tools.F.html
           ~title:"ocalc"
           ~css:[["css"; "ocalc.css"]]
           Html.F.(body [
             txt ("Bonjour mon ami " ^ prenom ^ " " ^ nom);
           ]))
