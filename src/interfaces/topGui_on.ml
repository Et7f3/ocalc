open Parser
open Moteur

(*boucle principal de l'interface initi� par GMain.main*)
let main (_, _) =
  let _ = GMain.init () in
  (*La fen�tre principale*)
  let window = GWindow.window
    ~decorated:true
    ~deletable:true
    ~height:250
    ~width:500
    ~resizable:true
    ~title:"Ocalc" () in

  let vbox = GPack.vbox
    ~border_width:5
    ~packing:window#add () in

  let hbox = GPack.hbox
    ~spacing:3
    ~border_width:5
    ~packing:(vbox#pack ~expand:false) () in

  (*Cr�ation de la zone d'entr�e + bouton de recherche*)
  let entry = GEdit.entry
    ~text:"Entrez votre formule"
    ~visibility:true
    ~editable:true
    ~packing:hbox#add () in

  let search = GButton.button
    ~label:"R�soudre"
    ~packing:(hbox#pack ~expand:false) () in

  (*toolbar et ses boutons*)
  let toolbar = GButton.toolbar
    ~orientation:`HORIZONTAL
    ~style:`TEXT
    ~tooltips:true
    ~packing:(hbox#pack ~expand:false) () in

  let _ (*toolb*) = GButton.tool_button
    ~label:"Menu principale"
    ~homogeneous:true
    ~expand:false
    ~show:false
    ~packing:(toolbar#insert ~pos:0) () in

  let _ (*toolb2*) = GButton.tool_button
    ~label:"Historique"
    ~homogeneous:true
    ~expand:false
    ~show:false
    ~packing:(toolbar#insert ~pos:1) () in

  let _ (*toolb3*) = GButton.tool_button
    ~label:"Solveur"
    ~homogeneous:true
    ~expand:false
    ~show:false
    ~packing:(toolbar#insert ~pos:2) () in

  let _ (*toolb4*) = GButton.tool_button
    ~label:"Option"
    ~homogeneous:true
    ~expand:false
    ~show:false
    ~packing:(toolbar#insert ~pos:3) () in

  let toolb5 = GButton.tool_button
    ~label:"Quitter"
    ~stock:`QUIT
    ~homogeneous:true
    ~expand:false
    ~show:true
    ~packing:(toolbar#insert ~pos:4) () in

  (*Buffer = text qui sera affich�*)
  let res =  GText.buffer
    ~text:"Solution :" () in

  (*Permet d'afficher le buffer*)
  let _ (*texte*) = GText.view
    ~buffer:res
    ~editable:false
    ~cursor_visible:false
    ~accepts_tab:false
    ~border_width:2
    ~packing:(vbox#pack ~expand:true) () in

  (*pour fermer le logicielle*)
  let _ = window#connect#destroy ~callback:GMain.quit in
  let _ = toolb5#connect#clicked ~callback:GMain.quit in

  (*r�cupartion de l'entr�e avec affichage du r�sultat*)
  let _ = search#connect#clicked ~callback:(fun () -> res#set_text ("Solution :\n"^ (texte_de_expr (eval (expr_de_texte entry#text))))) in
  (*montre la fen�tre*)
  window#show ();
  (*initie la boucle principale*)
  GMain.main ()
