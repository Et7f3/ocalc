let debug_mode =
  match Sys.getenv_opt "debug" with
    None -> false
  | Some x -> true

let faire_dossier name =
  if Sys.file_exists name then
    if not (Sys.is_directory name) then
      failwith (name ^ " existe déjà et n'est pas un dossier")
    else
      ()
  else
    Unix.mkdir name (Unix.stat "..").st_perm;;

let faire_dossiers =
  let rec faire_un_dossier = function
      [] -> ()
    | e :: l -> faire_dossier e; Unix.chdir e; faire_un_dossier l; Unix.chdir ".."
  in let rec faire_dossiers = function
        [] -> ()
      | e :: l -> faire_un_dossier e; faire_dossiers l
  in faire_dossiers

let executer_commande ligne_commande =
  let ret = Sys.command ligne_commande in
  if ret <> 0 then
    let () =
      Printf.printf "Cette ligne de commande à échoué:\r\n\r\n\t%s" ligne_commande in
    let () = Printf.printf "\r\n\r\net a retourné le code %d\r\n" ret in
    failwith "Impossible de continuer"

let modifie_valeur_dico clef valeur =
  let rec ajout acc = function
      [] -> acc
    | e :: l -> ajout (e :: acc) l
  in let rec l acc = function
        [] -> failwith "Composant non trouvé"
      | (cl, _) :: liste when cl = clef -> ajout ((cl, valeur) :: liste) acc
      | e :: liste -> l (e :: acc) liste
  in l []

type valeur_de_retour =
    Bien_fini
  | Argument_manquant of string
  | Argument_non_renonnu of string

let construire_objet nom_source nom_dest options source dest =
  let ligne_de_commande = "ocamlc -c " ^ options ^ " " ^ source ^ nom_source ^ " -o " ^ dest ^ nom_dest
  in let () = if debug_mode then print_endline ligne_de_commande in
  executer_commande ligne_de_commande

(*let s = construire_objet "type.mli" "type.cmi" "unix.cma -I +threads" "src/header/" "obj/header/"
*)


let rec gestionnaire_construire i argc argv =
  let (i, cible) =
    if i < argc && argv.(i) = "-cible" then
      let i = i + 1 in
      if i < argc then
        match argv.(i) with
          ("final" | "debug") as cible -> (i + 1, cible)
        | _ -> failwith "cible non prise en charge"
      else
        failwith "Cible non défini"
    else
      (i, "final")
  in let () = faire_dossiers [
      ["obj"; "noyau"];
      ["obj"; "header"];
      ["obj"; "on"];
      ["obj"; "off"];
      ["obj"; "interface"; "on"];
      ["obj"; "interface"; "off"];
      ["bin"; cible]
    ]
  in let command_line nom actif =
       let actif = if actif then "on/" else "off/" in
       "echo ocamlc -c unix.cma -I +threads src/" ^ actif ^ nom ^ ".mli -o obj/header/" ^ nom ^ ".cmi && " ^
       "echo ocamlc -c unix.cma -I +threads -I obj/header src/" ^ actif ^ nom ^ ".ml -o obj/" ^ actif ^ nom ^ ".cmo"
  in let module_principaux = ["type"; "utils"; "lexer"; "parser"] in
  let construire_principaux options source_suffix dest_suffix chemin_source chemin_dest =
    let rec construire_principaux = function
        [] -> ()
      | e :: l ->
        let (nom_source, nom_dest) = (e ^ source_suffix, e ^ dest_suffix) in
        let () = construire_objet nom_source nom_dest options chemin_source chemin_dest in
        construire_principaux l
    in construire_principaux
  in let () = construire_principaux "unix.cma -safe-string -I +threads -I obj/noyau" ".mli" ".cmi" "src/noyau/" "obj/noyau/" module_principaux in
  let () = construire_principaux "unix.cma -safe-string -I +threads -I obj/noyau" ".ml" ".cmo" "src/noyau/" "obj/noyau/" module_principaux in
  let interfaces = ["topCmd"] in
  let modules_interfaces = [
    "topCmd", true
  ]
  in let modules = [
    "grandEntier", true;
    (*"matrice", true;*)
    "serveur", false;
  ]
  in let rec consommer_argument i modules modules_interfaces =
       if i < argc && argv.(i) <> "--" then
         let arg = argv.(i) in
         let i = i + 1 in
         let valeur = match String.get arg 0 with
             '+' -> true
           | '-' -> false
           | _ -> failwith ("argument " ^ arg ^ " non compris")
         in let clef = String.sub arg 1 (String.length arg - 1) in
         if List.exists (fun nom -> nom = clef) module_principaux then
           failwith (clef ^ " est un module principal")
         else if List.exists (fun nom -> nom = clef) interfaces then
           consommer_argument i modules (modifie_valeur_dico clef valeur modules_interfaces)
        else
           consommer_argument i (modifie_valeur_dico clef valeur modules) modules_interfaces
       else
         i, modules, modules_interfaces
  in let (i, modules, modules_interfaces) = consommer_argument i modules modules_interfaces in
  let rec l = function
      [] -> ()
    | (nom, actif) :: liste ->
      let _ = executer_commande (command_line nom actif) in
      l liste
  in let () = l modules in
  let string_join traiter =
    let rec string_join acc = function
        [] -> acc
      | e :: l -> string_join (acc ^ (traiter e)) l
    in string_join
  in let ligne_finale = string_join (fun e -> " obj/noyau/" ^ e ^ ".cmo") "" module_principaux in
  let ligne_finale = string_join (fun (e, actif) ->
      let chemin = "interface/" ^ (if actif then "on/" else "off/") in
      let () = construire_principaux "unix.cma -safe-string -I +threads -I obj/noyau" ".ml" ".cmo" ("src/" ^ chemin) ("obj/" ^ chemin) [e] in
      " obj/" ^ chemin ^ e ^ ".cmo") ligne_finale modules_interfaces in
  let rec l = function
      [] -> ()
    | (nom, actif) :: liste -> ()
  in let () = l modules_interfaces in
  let () = executer_commande ("ocamlc -o bin/" ^ cible ^ "/main.exe" ^ ligne_finale) in
  i, Bien_fini

and gestionnaire_tester i argc argv =
  i, Bien_fini

and gestionnaire_nettoyer i argc argv =
  let () = executer_commande "del /S /Q *.cm*" in
  i, Bien_fini

and gestionnaire_aide i argc argv =
  let afficher_utilisation (alias, nom, _, help_msg, usage_msg) =
    let () = print_string "Utilisations: " in
    let () = print_string argv.(0) in
    let () = print_char ' ' in
    let () = print_string nom in
    let () = print_char ' ' in
    let () = print_string usage_msg in
    let () = print_string "\r\n\t" in
    print_endline help_msg
  in if i < argc && argv.(i) <> "--" then
    let caterogie = argv.(i) in
    let i = i + 1 in
    let rec l = function
        [] -> Argument_non_renonnu caterogie
      | (alias, nom, _, help_msg, usage_msg) as f :: _
        when nom = caterogie ->
        let () = afficher_utilisation f in
        Bien_fini
      | _ :: liste -> l liste
    in i, (l liste_de_sous_commande)
  else
    let rec l = function
        [] -> ()
      | (_, nom, _, help_msg, _) :: liste ->
        Printf.printf "%-20s %s\n" nom help_msg;
        l liste
    in let () = l liste_de_sous_commande in
    let () = print_string "\r\n" in
    i, Bien_fini

and liste_de_sous_commande = [
  "-c", "construire", gestionnaire_construire, "Construit OCalc", "[-cible {final|debug}]";
  "-a", "aider", gestionnaire_aide, "Affiche l'aide sur toute les fonctions", "[caterogie={toute|nom_de_sous_commande}]";
  "-t", "tester", gestionnaire_tester, "Lance la série de test unitaire", "";
  "-n", "nettoyer", gestionnaire_nettoyer, "Nettoie tout les fichiers intermédiaires", ""];;

exception Commande_non_trouve of string

let main argc argv =
  let rec switch i = function
      [] -> raise (Commande_non_trouve argv.(i))
    | (alias, nom, gestionnaire, _, _) :: _ when nom = argv.(i) || alias = argv.(i) ->
      let (i, _) = gestionnaire (i + 1) argc argv in
      (*let i = i + 1 in*)
      if i < argc then
        if argv.(i) = "--" then
          switch (i + 1) liste_de_sous_commande
        else
          switch i liste_de_sous_commande
    | _ :: l -> switch i l
  in if argc > 1 then
    switch 1 liste_de_sous_commande
  else
    print_endline "aucun argument à analyser"

let () =
  let argv = Sys.argv in
  let argc = Array.length argv in
  main argc argv
