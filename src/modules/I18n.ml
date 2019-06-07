type lang =
    Francais
  | Anglais

let lang = ref Francais

let definir_lang l =
  lang := l

let obtenir_lang () =
  match !lang with
    Francais -> "fr"
  | Anglais -> "eng"

let division_par_zero () =
  match !lang with
    Francais -> "Division par zéro à eu lieu"
  | Anglais -> "Division by zero occur"

let separateur_unite () =
  match !lang with
    Francais -> ","
  | Anglais -> "."

let impossible_convertir_pas_affectable () =
  match !lang with
    Francais -> "Impossible de convertir Pas_affectable"
  | Anglais -> "We can't convert Pas_affectable"

let impossible_convertir_vide () =
  match !lang with
    Francais -> "Impossible de convertir Vide"
  | Anglais -> "We can't convert Vide"

let matrice_mauvaise_dimension () =
  match !lang with
    Francais -> "matrice de mauvaise dimension"
  | Anglais -> "Matrix don't have right size"

let fichier_inexistant fichier =
  match !lang with
    Francais -> fichier ^ " n'existe pas"
  | Anglais -> fichier ^ " doesn't exist"

let fichier_pas_math fichier =
  match !lang with
    Francais -> fichier ^ " n'as pas l'extension .math"
  | Anglais -> fichier ^ " isn't a .math file"

let bienvenue () =
  match !lang with
    Francais -> "Bienvenue chez OCalc"
  | Anglais -> "Walcome at OCalc"

let sortir_msg () =
  match !lang with
    Francais -> "tapez sortir ou quitter pour quitter"
  | Anglais -> "type exit or quit to quit"

let sortir_1 () =
  match !lang with
    Francais -> "sortir"
  | Anglais -> "exit"

let sortir_2 () =
  match !lang with
    Francais -> "quitter"
  | Anglais -> "quit"

let mauvais_parenthesage () =
  match !lang with
    Francais -> "Vérifiez vos parenthèses !!!"
  | Anglais -> "Some parenthesis mismatch !!!"

let erreur_de_syntaxe () =
  match !lang with
    Francais -> "Erreur de syntaxe"
  | Anglais -> "Syntax error"

let definition_valide () =
  match !lang with
    Francais -> "Définition valide"
  | Anglais -> "Definition valid"

let retour () =
  match !lang with
    Francais -> "Retour"
  | Anglais -> "Back"

let calculer () =
  match !lang with
    Francais -> "Calculer"
  | Anglais -> "Compute"

let effacer () =
  match !lang with
    Francais -> "Effacer"
  | Anglais -> "Erase"

let menu_historique () =
  match !lang with
    Francais -> "Historique"
  | Anglais -> "History"

let menu_accueil () =
  match !lang with
    Francais -> "Accueil"
  | Anglais -> "Home"

let menu_calcul () =
  match !lang with
    Francais -> "Calcul"
  | Anglais -> "Calculus"

let menu_matrices () =
  match !lang with
    Francais -> "Matrices"
  | Anglais -> "Matrix"

let menu_equations () =
  match !lang with
    Francais -> "Équations"
  | Anglais -> "Solver"

let contractition_presente () =
  match !lang with
    Francais -> "Il y a une contradiction présente"
  | Anglais -> "Solution don't exist"

let definition_non_autorise () =
  match !lang with
    Francais -> "Vous ne pouvez pas définir de variable ici"
  | Anglais -> "You can't define value here"
