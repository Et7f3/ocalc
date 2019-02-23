  let faire_dossier name =
    if Sys.file_exists name then
      if not (Sys.is_directory name) then
        failwith (name ^ " existe déjà et n'est pas un dossier")
      else
        ()
    else
      Unix.mkdir name 777

let faire_dossiers =
  let rec faire_un_dossier = function
      [] -> ()
    | e :: l -> faire_dossier e; Unix.chdir e; faire_un_dossier l; Unix.chdir ".."
  in let rec faire_dossiers = function
    [] -> ()
  | e :: l -> faire_un_dossier e; faire_dossiers l
  in faire_dossiers

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

let rec gestionnaire_construire i argc argv =
  i, Bien_fini

and gestionnaire_tester i argc argv =
  i, Bien_fini

and gestionnaire_nettoyer i argc argv =
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
  "-c", "construire", gestionnaire_construire, "Construit OCalc", "[target={final|debug}]";
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
