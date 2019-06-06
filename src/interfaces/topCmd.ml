let corp_principal contexte =
  let ps1 = "# " in
  let () = print_endline "Bonjour chez OCalc" in
  let () = print_endline "type exit to extit" in
  let rec evalue_en_boucle ancienne_ligne contexte =
    let () = print_string ps1 in
    let ligne = ancienne_ligne ^ read_line () in
    if Filename.check_suffix ligne ";;" then
      let ligne =
        let taille = String.length ligne - 2 in
        String.sub ligne 0 taille
      in
      if ligne <> "exit" || ligne <> "quit" then
        let sortie, contexte =
          if Noyau.Utils.parenthese_correcte ligne then
            try
              Noyau.Moteur.evaluate_with_history ligne contexte
            with Division_by_zero -> "Division par zéro", contexte
          else
            "vérifiez vos parenthèses !!!", contexte
      in let () = print_endline sortie in
      evalue_en_boucle "" contexte
    else
      evalue_en_boucle ligne contexte
  in evalue_en_boucle "" contexte
in corp_principal Commune.init_context
