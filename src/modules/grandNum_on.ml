type num =
    E of GrandEntier_on.grandentier
  | R of GrandReel_on.grandreel
  | Q of GrandRationnel_on.grandrationnel

let powerabs c =
  let rec pa i c = match c with
      0 -> i
    | _ -> pa (0 :: i) (c-1)
  in pa [1] (abs c)

let rec up (a, b, c) =
  if c <= 0 then
    (a, b, [1])
  else
    up (a, 0 :: b, c - 1)


let truc (a, b, c) =
  if c = 0 then
    (a, b, [1])
  else if c > 0 then
    up (a, b, c)
  else
    a, b, powerabs c



let multiplier = function
  E e1, E e2 -> E (GrandEntier_on.multiplier e1 e2)
  | R r1, R r2 -> R (GrandReel_on.multiplier r1 r2)
  | Q q1, Q q2 -> Q (GrandRationnel_on.multiplier q1 q2)
  | R r2, E (signe, e1) -> R (GrandReel_on.multiplier (signe, e1, 0) r2)
  | E (signe, e1), R r2 -> R (GrandReel_on.multiplier (signe, e1, 0) r2)
  | E (signe, e1), Q q1 -> Q (GrandRationnel_on.multiplier q1 (signe, e1, [1]))
  | Q q1, E (signe, e1) -> Q (GrandRationnel_on.multiplier q1 (signe, e1, [1]))
  | R r1, Q q1 -> Q (GrandRationnel_on.multiplier q1 (truc r1))
  | Q q1, R r1 -> Q (GrandRationnel_on.multiplier q1 (truc r1))


let soustraire = function
    E e1, E e2 -> E (GrandEntier_on.soustraire e1 e2)
  | R r1, R r2 -> R (GrandReel_on.soustraire r1 r2)
  | Q q1, Q q2 -> Q (GrandRationnel_on.soustraire q1 q2)
  | R r2, E (signe, e1) -> R (GrandReel_on.soustraire (signe, e1, 0) r2)
  | E (signe, e1), R r2 -> R (GrandReel_on.soustraire (signe, e1, 0) r2)
  | E (signe, e1), Q q1 -> Q (GrandRationnel_on.soustraire q1 (signe, e1, [1]))
  | Q q1, E (signe, e1) -> Q (GrandRationnel_on.soustraire q1 (signe, e1, [1]))
  | R r1, Q q1 -> Q (GrandRationnel_on.soustraire  q1 (truc r1))
  | Q q1, R r1 -> Q (GrandRationnel_on.soustraire  q1 (truc r1))


let additioner = function
    E e1, E e2 -> E (GrandEntier_on.additioner e1 e2)
  | R r1, R r2 -> R (GrandReel_on.additioner r1 r2)
  | Q q1, Q q2 -> Q (GrandRationnel_on.additioner q1 q2)
  | R r2, E (signe, e1) -> R (GrandReel_on.additioner (signe, e1, 0) r2)
  | E (signe, e1), R r2 -> R (GrandReel_on.additioner (signe, e1, 0) r2)
  | E (signe, e1), Q q1 -> Q (GrandRationnel_on.additioner q1 (signe, e1, [1]))
  | Q q1, E (signe, e1) -> Q (GrandRationnel_on.additioner q1 (signe, e1, [1]))
  | R r1, Q q1 -> Q (GrandRationnel_on.additioner q1 (truc r1))
  | Q q1, R r1 -> Q (GrandRationnel_on.additioner q1 (truc r1))



let diviser = function
    E (signe1, e1), E (signe2, e2) -> Q (signe1 != signe2, e1, e2)
  | R r1, R r2 -> Q (GrandRationnel_on.diviser (truc r1) (truc r2))
  | Q q1, Q q2 -> Q (GrandRationnel_on.diviser q1 q2)
  | R r2, E (signe, e1) -> Q (GrandRationnel_on.diviser (signe, e1, [1]) (truc r2))
  | E (signe, e1), R r2 -> Q (GrandRationnel_on.diviser (signe, e1, [1]) (truc r2))
  | E (signe, e1), Q q1 -> Q (GrandRationnel_on.diviser q1 (signe, e1, [1]))
  | Q q1, E (signe, e1) -> Q (GrandRationnel_on.diviser q1 (signe, e1, [1]))
  | R r1, Q q1 -> Q (GrandRationnel_on.diviser q1 (truc r1))
  | Q q1, R r1 -> Q (GrandRationnel_on.diviser q1 (truc r1))
