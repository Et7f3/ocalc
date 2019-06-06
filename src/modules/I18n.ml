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