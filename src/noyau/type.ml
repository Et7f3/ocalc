open Lien

type expr = Variable of string
          | Entier of GrandEntier.grandentier
          | Textenonvalide of string
          | Operation of string * expr list
          | Neg of expr
          | Inv of expr
