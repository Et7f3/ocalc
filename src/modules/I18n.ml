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
