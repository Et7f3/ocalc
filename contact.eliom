[%%shared
    open Eliom_lib
    open Eliom_content
    open Html.F
]

let page () () =
  Lwt.return
        (Eliom_tools.F.html
           ~title:"ocalc"
           ~css:[["css"; "ocalc.css"]]
           Html.F.(body [
             Menu.contenu;
             h1 [txt "Contact"];
           ]))
