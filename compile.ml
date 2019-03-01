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
    Unix.mkdir name (Unix.stat "..").st_perm

let faire_dossiers =
  let rec faire_un_dossier = function
      [] -> ()
    | e :: l -> faire_dossier e; Unix.chdir e; faire_un_dossier l; Unix.chdir ".."
  in let rec faire_dossiers = function
        [] -> ()
      | e :: l -> faire_un_dossier e; faire_dossiers l
  in faire_dossiers

let executer_commande ligne_commande vraiment =
  let () = if debug_mode then print_endline ligne_commande in
  if vraiment then
    let ret = Sys.command ligne_commande in
    if ret <> 0 then
      let () =
        Printf.printf "Cette ligne de commande à échoué:\r\n\r\n\t%s" ligne_commande in
      let () = Printf.printf "\r\n\r\net a retourné le code %d\r\n" ret in
      failwith "Impossible de continuer"

let modifie_valeur_dico clef f =
  let rec ajout acc = function
      [] -> acc
    | e :: l -> ajout (e :: acc) l
  in let rec l acc = function
        [] -> failwith "Composant non trouvé"
      | (cl, valeur) :: liste when cl = clef -> ajout ((cl, f valeur) :: liste) acc
      | e :: liste -> l (e :: acc) liste
  in l []

let construire_objet vraiment compilateur options source dest deps =
  let analasye_dep e =
    (*Sys.file_exists e && *)
    ((Unix.stat e).st_mtime > (Unix.stat dest).st_mtime)
  in let rec analasye_deps = function
        [] -> false
      | e :: l -> analasye_dep e || analasye_deps l
  in let besoin_reconstruire =
       not (Sys.file_exists dest) || analasye_deps deps
  in if besoin_reconstruire then
    executer_commande (compilateur ^ options ^ " " ^ source ^ " -o " ^ dest) vraiment


type valeur_de_retour =
    Bien_fini
  | Argument_manquant of string
  | Argument_non_renonnu of string
  | Valeur_non_renonnu of string

let rec gestionnaire_construire i argc argv =
  let vraiment = ref true in
  let cible = ref "final" in
  let rec consommer_argument i =
    if i < argc then
      match argv.(i) with
        "--" -> i, Bien_fini
      | "-cible" ->
        let i = i + 1 in
        if i < argc then
          match argv.(i) with
            "final" | "debug" as e ->
            let () = cible := e in
            let i = i + 1 in
            consommer_argument i
          | arg -> i, Valeur_non_renonnu arg
        else
          i, Argument_manquant "-cible"
      | "-n" ->
        let () = vraiment := not !vraiment in
        let i = i + 1 in
        consommer_argument i
      | arg -> i, Argument_non_renonnu arg
    else
      i, Bien_fini
  in let i, ret = consommer_argument i in
  if ret = Bien_fini then
    let modules = ref [

      ]
    in let rec consommer_argument i =
         if i < argc then
           match argv.(i) with
             "--" -> i, Bien_fini
           | name when name.[0] = '+' || name.[0] = '-' ->
             i, Bien_fini
           | arg -> i, Argument_non_renonnu arg
         else
           i, Bien_fini
    in let construire_objet = construire_objet !vraiment in
    i, ret
  else
    i, ret

and gestionnaire_tester i argc argv =
  i, Bien_fini

and gestionnaire_nettoyer i argc argv =
  i, Bien_fini

and gestionnaire_aide i argc argv =
  i, Bien_fini

and liste_de_sous_commande = [
  "-c", "construire", gestionnaire_construire, "Construit OCalc", "[-cible {final|debug}]";
  "-a", "aider", gestionnaire_aide, "Affiche l'aide sur toute les fonctions", "[caterogie={toute|nom_de_sous_commande}]";
  "-t", "tester", gestionnaire_tester, "Lance la série de test unitaire", "";
  "-n", "nettoyer", gestionnaire_nettoyer, "Nettoie tout les fichiers intermédiaires", ""]

exception Commande_non_trouve of string
exception Erreur_Sous_Module of valeur_de_retour
let main argc argv =
  let rec switch i = function
      [] -> raise (Commande_non_trouve argv.(i))
    | (alias, nom, gestionnaire, _, _) :: _ when nom = argv.(i) || alias = argv.(i) ->
      let (i, ret) = gestionnaire (i + 1) argc argv in
      (*let i = i + 1 in*)
      if ret <> Bien_fini then
        raise (Erreur_Sous_Module ret)
      else if i < argc then
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
