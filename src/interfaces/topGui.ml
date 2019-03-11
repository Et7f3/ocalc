open Parser
open Moteur

let _ = GMain.init ()

(*La fenêtre principale*)
let window = GWindow.window
    ~decorated:true
    ~deletable:true
    ~height:250
    ~width:500
    ~resizable:true
    ~title:"Ocalc" ()

let vbox = GPack.vbox
    ~border_width:5
    ~packing:window#add ()

let hbox = GPack.hbox
    ~spacing:3
    ~border_width:5
    ~packing:(vbox#pack ~expand:false) ()

(*Création de la zone d'entrée + bouton de recherche*)
let entry = GEdit.entry
    ~text:"Entrez votre formule"
    ~visibility:true
    ~editable:true
    ~packing:hbox#add ()

let search = GButton.button
    ~label:"Résoudre"
    ~packing:(hbox#pack ~expand:false) ()

(*toolbar et ses boutons*)
let toolbar = GButton.toolbar
    ~orientation:`HORIZONTAL
    ~style:`TEXT
    ~tooltips:true
    ~packing:(hbox#pack ~expand:false) ()

let _ (*toolb*) = GButton.tool_button
    ~label:"Menu principale"
    ~homogeneous:true
    ~expand:false
    ~show:false
    ~packing:(toolbar#insert ~pos:0) ()

let _ (*toolb2*) = GButton.tool_button
    ~label:"Historique"
    ~homogeneous:true
    ~expand:false
    ~show:false
    ~packing:(toolbar#insert ~pos:1) ()

let _ (*toolb3*) = GButton.tool_button
    ~label:"Solveur"
    ~homogeneous:true
    ~expand:false
    ~show:false
    ~packing:(toolbar#insert ~pos:2) ()

let _ (*toolb4*) = GButton.tool_button
    ~label:"Option"
    ~homogeneous:true
    ~expand:false
    ~show:false
    ~packing:(toolbar#insert ~pos:3) ()

let toolb5 = GButton.tool_button
    ~label:"Quitter"
    ~stock:`QUIT
    ~homogeneous:true
    ~expand:false
    ~show:true
    ~packing:(toolbar#insert ~pos:4) ()

(*Buffer = text qui sera affiché*)
let res =  GText.buffer
    ~text:"Résultat:" ()

(*Permet d'afficher le buffer*)
let _ (*texte*) = GText.view
    ~buffer:res
    ~editable:false
    ~cursor_visible:false
    ~accepts_tab:false
    ~border_width:2
    ~packing:(vbox#pack ~expand:true) ()

(*boucle principal de l'interface initié par GMain.main*)
let _ =
  (*pour fermer le logicielle*)
  let _ = window#connect#destroy ~callback:GMain.quit in
  let _ = toolb5#connect#clicked ~callback:GMain.quit in
  (*récupartion de l'entrée avec affichage du résultat*)
  let _ = search#connect#clicked ~callback:(fun () -> res#set_text ("Résultat:\n" ^ (texte_de_expr (eval (expr_de_texte entry#text))))) in
  (*montre la fenêtre*)
  window#show ();
  (*initie la boucle principale*)
  GMain.main ()
