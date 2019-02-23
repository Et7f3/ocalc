type grandentier = int * int list
type expr =
    Variable of string
  | Entier of grandentier
  | Textenonvalide of string
