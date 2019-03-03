let debug_mode =
  match Sys.getenv_opt "debug" with
    None -> false
  | Some x -> true

let faire_dossier nom =
  if Sys.file_exists nom then
    if not (Sys.is_directory nom) then
      failwith (nom ^ " existe déjà et n'est pas un dossier")
    else
      ()
  else
    Unix.mkdir nom (Unix.stat "..").st_perm

let faire_dossiers =
  let rec faire_un_dossier = function
      [] -> ()
    | e :: l -> faire_dossier e; Unix.chdir e; faire_un_dossier l; Unix.chdir ".."
  in let rec faire_dossiers = function
        [] -> ()
      | e :: l -> faire_un_dossier e; faire_dossiers l
  in faire_dossiers

let commande_en_attentes = ref []

let executer_commande ligne_commande vraiment =
  let () = if debug_mode || not vraiment then print_endline ligne_commande in
  if vraiment then
    commande_en_attentes := ligne_commande :: !commande_en_attentes

let executer_commandes () =
  let rec boucle = function
      [] -> commande_en_attentes := []
    | ligne_commande :: liste ->
      let ret = Sys.command ligne_commande in
      let () =
        if ret <> 0 then
          let () =
            Printf.printf "Cette ligne de commande à échoué:\r\n\r\n\t%s" ligne_commande in
          let () = Printf.printf "\r\n\r\net a retourné le code %d\r\n" ret in
          failwith "Impossible de continuer"
      in boucle liste
  in boucle (List.rev !commande_en_attentes)

let modifie_valeur_dico clef f =
  let rec ajout acc = function
      [] -> acc
    | e :: l -> ajout (e :: acc) l
  in let rec l acc = function
        [] -> failwith "Composant non trouvé"
      | (cl, valeur) :: liste when cl = clef -> ajout ((cl, f valeur) :: liste) acc
      | e :: liste -> l (e :: acc) liste
  in l []

let rec est_clef_dico clef = function
    [] -> false
  | (e, _) :: l -> e = clef || est_clef_dico clef l

let rec valeur_clef_dico clef = function
    [] -> failwith "Composant non trouvé"
  | (e, _) as elt :: l ->
    if e = clef then
      elt
    else
      valeur_clef_dico clef l

let construire_objet vraiment compilateur options =
  let ligne_commande = compilateur ^ " " ^ options ^ " " in
  let construire_objet source dest deps =
    let analasye_dep e =
      (*Sys.file_exists e && *)
      ((Unix.stat e).st_mtime > (Unix.stat dest).st_mtime)
    in let rec analasye_deps = function
          [] -> false
        | e :: l -> analasye_dep e || analasye_deps l
    in let besoin_reconstruire =
         not (Sys.file_exists dest) || analasye_deps deps
    in if besoin_reconstruire then
      executer_commande (ligne_commande ^ source ^ " -o " ^ dest) vraiment
  in construire_objet

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
          | arg -> if arg.[0] = '-' then
              i, Argument_manquant "-cible"
            else
              i, Valeur_non_renonnu arg
        else
          i, Argument_manquant "-cible"
      | "-n" ->
        let () = vraiment := not !vraiment in
        let i = i + 1 in
        consommer_argument i
      | arg -> i, Bien_fini (*Argument_non_renonnu arg*)
    else
      i, Bien_fini
  in let i, ret = consommer_argument i in
  if ret = Bien_fini then
    let modules = ref [
        "grandEntier", true; (* calcul avec des entier de précision infini *)
      ]
    in let interfaces = ref [
        "topCmd", true; (* lis simplement l'entrée standard et l'évalue *)
      ]
    in let modules_noyau = [
        "type";
        "utils";
        "lexer";
        "parser";
      ]
    in let rec consommer_argument i =
         if i < argc then
           match argv.(i) with
             "--" -> i, Bien_fini
           | nom when nom.[0] = '+' || nom.[0] = '-' ->
             let clef = String.sub nom 1 (String.length nom - 1) in
             let i = i + 1 in
             let (i, ret) =
               if est_clef_dico clef !interfaces then
                 let () = interfaces := modifie_valeur_dico clef (fun (_) -> nom.[0] = '+') !interfaces
                 in consommer_argument i
               else if est_clef_dico clef !modules then
                 let () = modules := modifie_valeur_dico clef (fun (_) -> nom.[0] = '+') !modules
                 in consommer_argument i
               else
                 failwith (clef ^ " n'est ni un module ni une interface")
             in i, ret
           | arg -> i, Argument_non_renonnu arg
         else
           i, Bien_fini
    in
    let i, ret = consommer_argument i in
    let () = faire_dossiers [
        ["bin"; !cible];
        ["obj"; "noyau"];
        ["obj"; "modules"];
        ["obj"; "interfaces"];
      ]
    in let options =
         let otpt =  "-c -safe-string -I obj/modules"
         in if !cible  = "debug" then
           otpt ^ " -g"
         else
           otpt
    in let construire_objet1 = construire_objet !vraiment "ocamlc" options in (* modules *)
    let options = options ^ " -I obj/noyau" in
    let construire_objet2 = construire_objet !vraiment "ocamlc" (options ^ " -I obj obj/lien.cmo -open Lien") in (* noyau *)
    let options = options ^ " -I obj/interfaces" in
    let construire_objet3 = construire_objet !vraiment "ocamlc" options in (* interfaces *)
    let options = options ^ " unix.cma -I +threads threads.cma -I obj" in
    let construire_objet4 = construire_objet !vraiment "ocamlc" (String.sub options 3 (String.length options - 3)) in
    let fichiers = ref "" in
    let fichier = open_out "obj/lien.ml" in
    let lier ?fichier_o nom actif =
      let nom =
        match nom.[0] with
        'a' .. 'z' ->
          let nom = Bytes.of_string nom in
          let nom = Bytes.capitalize_ascii nom in
          let nom = Bytes.to_string nom in
          nom
        | _ -> nom
      in
      match fichier_o with
        None -> Printf.fprintf fichier "module %s = %s\n" nom (nom ^ if actif then "_on" else "_off")
      | Some fichier -> Printf.fprintf fichier "module %s = %s\n" nom (nom ^ if actif then "_on" else "_off")
    in
    let rec boucle = function
        [] -> ()
      | (nom, actif) :: liste ->
        let () = lier nom actif in
        let src = "src/modules/" ^ nom ^ if actif then "_on.ml" else "_off.ml" in
        let dst = "obj/modules/" ^ nom ^ if actif then "_on.cm" else "_off.cm" in
        let dest_o = dst ^ "o" in
        let () = fichiers := !fichiers ^  dest_o ^ " " in
        let () = construire_objet1 (src ^ "i") (dst ^ "i") [] in
        let () = construire_objet1 src dest_o [] in
        boucle liste
    in let () = boucle !modules in
    let () = close_out fichier in
    let () = construire_objet3 "obj/lien.ml" "obj/lien.cmi" [] in
    let () = construire_objet3 "obj/lien.ml" "obj/lien.cmo" [] in
    let () = fichiers := !fichiers ^ "obj/lien.cmo " in
    let fichier = open_out "obj/lien_intf.ml" in
    let rec boucle = function
        [] -> ()
      | nom :: liste ->
        let src = "src/noyau/" ^ nom ^ ".ml" in
        let dst = "obj/noyau/" ^ nom ^ ".cm" in
        let dest_o = dst ^ "o" in
        let () = fichiers := !fichiers ^ dest_o ^ " " in
        let () = construire_objet2 (src ^ "i") (dst ^ "i") [] in
        let () = construire_objet2 src dest_o [] in
        boucle liste
    in let () = boucle modules_noyau in
    let rec boucle = function
        [] -> ()
      | (nom, actif) :: liste ->
        let () = lier ~fichier_o:fichier nom actif in
        let src = "src/interfaces/" ^ nom ^ (if actif then "_on.ml" else "_off.ml") in
        let dst = "obj/interfaces/" ^ nom ^ (if actif then "_on.cm" else "_off.cm") in
        let dest_o = dst ^ "o" in
        let () = fichiers := !fichiers ^ dest_o ^ " " in
        let () = construire_objet3 (src ^ "i") (dst ^ "i") [] in
        let () = construire_objet3 src dest_o [] in
        boucle liste
    in let () = boucle !interfaces in
    let () = close_out fichier in
    let () = construire_objet3 "obj/lien_intf.ml" "obj/lien_intf.cmi" [] in
    let () = construire_objet3 "obj/lien_intf.ml" "obj/lien_intf.cmo" [] in
    let nom_final = "bin/" ^ !cible ^ "/final.exe" in
    let () = construire_objet4 (!fichiers ^ "-open Lien_intf obj/lien_intf.cmo src/main.ml") nom_final [] in
    let () = executer_commandes () in
    let () =
      if !vraiment then
        print_endline ("L'executable a été généré avec succès ici: " ^ nom_final)
    in if ret = Bien_fini then
      i, Bien_fini
    else
      i, ret
  else
    i, ret

and gestionnaire_tester i argc argv =
  i, Bien_fini

and gestionnaire_nettoyer i argc argv =
  let rec boucle_dossiers dossier =
    let dossiers = Sys.readdir dossier in
    let () = Sys.chdir dossier in
    let rec boucle_dossiers_rec i =
      if i > 0 then
        let i = i - 1 in
        let () =
          if Sys.is_directory dossiers.(i) then
            boucle_dossiers dossiers.(i)
          else
            Sys.remove dossiers.(i)
        in boucle_dossiers_rec i
    in let () = boucle_dossiers_rec (Array.length dossiers) in
    Sys.chdir ".."
  in let () = boucle_dossiers "obj" in
  let () = boucle_dossiers "bin" in
  i, Bien_fini

and gestionnaire_aider i argc argv =
  let afficher_utilisation (nom, (alias, _, help_msg, usage_msg)) =
    let () = print_string "Utilisations: " in
    let () = print_string argv.(0) in
    let () = print_char ' ' in
    let () = print_string nom in
    let () =
      if usage_msg <> "" then
        let () = print_char ' ' in
        print_string usage_msg
    in let () = print_string "\r\n\t" in
    print_endline help_msg
  in let rec afficher_utilisations = function
        [] -> ()
      | e :: l ->
        let () = afficher_utilisation e in
        afficher_utilisations l
  in let derniere_fonction = ref [] in
  let rec aide_generale = function
      [] -> ()
    | (nom, (_, _, help_msg, _)) :: liste ->
      let () = Printf.eprintf "%-20s %s\n" nom help_msg
      in aide_generale liste
  in let rec consommer_argument i =
       if i = argc then
         i, Bien_fini
       else
         match argv.(i) with
           "--" -> i, Bien_fini
         | nom when est_clef_dico nom liste_de_sous_commande ->
           let i = i + 1 in
           let () = derniere_fonction := valeur_clef_dico nom liste_de_sous_commande :: !derniere_fonction in
           consommer_argument i
         | arg -> i, Valeur_non_renonnu arg
  in let i, ret = consommer_argument i in
  if ret = Bien_fini then
    if !derniere_fonction = [] then
      let () = aide_generale liste_de_sous_commande in
      i, Bien_fini
    else
      let () = afficher_utilisations !derniere_fonction in
      i, Bien_fini
  else
    let () = aide_generale liste_de_sous_commande in
    i, ret

and liste_de_sous_commande = [
  "construire", ("-c", gestionnaire_construire, "Construit OCalc", "[-n] [-cible {final|debug}] {+moduleActif|-moduleInactif} ...");
  "aider", ("-a", gestionnaire_aider, "Affiche l'aide sur toute les fonctions", "[(nom_sous_commande ...)]");
  "tester", ("-t", gestionnaire_tester, "Lance la série de test unitaire", "");
  "nettoyer", ("-n", gestionnaire_nettoyer, "Nettoie tout les fichiers intermédiaires", "")]

exception Commande_non_trouve of string
exception Erreur_Sous_Module of valeur_de_retour

let main argc argv =
  let rec switch i = function
      [] -> raise (Commande_non_trouve argv.(i))
    | (nom, (alias, gestionnaire, _, _)) :: _ when nom = argv.(i) || alias = argv.(i) ->
      let (i, ret) = gestionnaire (i + 1) argc argv in
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
