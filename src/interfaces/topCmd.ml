let corp_principal contexte =
  let ps1 = "# " in
  let () = print_endline (I18n.bienvenue ()) in
  let () = print_endline (I18n.sortir_msg ()) in
  let rec evalue_en_boucle ancienne_ligne contexte =
    let () = print_string ps1 in
    let ligne = ancienne_ligne ^ read_line () in
    if Filename.check_suffix ligne ";;" then
      let ligne =
        let taille = String.length ligne - 2 in
        String.sub ligne 0 taille
      in
      if ligne <> I18n.sortir_1 () || ligne <> I18n.sortir_2 () then
        let sortie, contexte =
          if Noyau.Utils.parenthese_correcte ligne then
            try
              Noyau.Moteur.evaluate_with_history ligne contexte
            with Division_by_zero -> (I18n.division_par_zero ()), contexte
          else
            I18n.mauvais_parenthesage (), contexte
      in let () = print_endline sortie in
      evalue_en_boucle "" contexte
    else
      evalue_en_boucle ligne contexte
  in evalue_en_boucle "" contexte
in corp_principal Commune.init_context
