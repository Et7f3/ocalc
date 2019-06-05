type num =
    E of GrandEntier_on.grandentier
  | R of GrandReel_on.grandreel
  | Q of GrandRationnel_on.grandrationnel

let power c =
  let rec p i = function
      0 -> i
    | c -> p (0 :: i) (c - 1)
  in p [1] c

let rec up (a, b, c) =
  if c <= 0 then
    a, b, [1]
  else
    up (a, 0 :: b, c - 1)

let q_depuis_r (a, b, c) =
  if c = 0 then
    a, b, [1]
  else if c > 0 then
    up (a, b, c)
  else
    a, b, power ~-c

let zero = E (GrandEntier_on.grandentier_depuis_texte "0")

let unit = E (GrandEntier_on.grandentier_depuis_texte "1")

let vers_grandentier = function
    E _ as e -> e
  | R (_, [], _) -> zero
  | R (s, mantisse, exp) as r ->
    if exp > 0 then
      let s, mantisse, exp = up (s, mantisse, exp) in
      if exp = [1] then (* Should always be true *)
        E (s, mantisse)
      else
        r
    else
      r
  | Q (_, [], _) -> zero
  | Q (signe, nom, [1]) -> E (signe, nom)
  | Q _ as q -> q

let multiplier = function
    E e1, E e2 -> E (GrandEntier_on.multiplier e1 e2)
  | R r1, R r2 -> R (GrandReel_on.multiplier r1 r2)
  | Q q1, Q q2 -> Q (GrandRationnel_on.multiplier q1 q2)
  | R r2, E (signe, e1) -> R (GrandReel_on.multiplier (signe, e1, 0) r2)
  | E (signe, e1), R r2 -> R (GrandReel_on.multiplier (signe, e1, 0) r2)
  | E (signe, e1), Q q1 -> Q (GrandRationnel_on.multiplier q1 (signe, e1, [1]))
  | Q q1, E (signe, e1) -> Q (GrandRationnel_on.multiplier q1 (signe, e1, [1]))
  | R r1, Q q1 -> Q (GrandRationnel_on.multiplier q1 (q_depuis_r r1))
  | Q q1, R r1 -> Q (GrandRationnel_on.multiplier q1 (q_depuis_r r1))

let multiplier e = multiplier e |> vers_grandentier

let soustraire = function
    E e1, E e2 -> E (GrandEntier_on.soustraire e1 e2)
  | R r1, R r2 -> R (GrandReel_on.soustraire r1 r2)
  | Q q1, Q q2 -> Q (GrandRationnel_on.soustraire q1 q2)
  | R r2, E (signe, e1) -> R (GrandReel_on.soustraire (signe, e1, 0) r2)
  | E (signe, e1), R r2 -> R (GrandReel_on.soustraire (signe, e1, 0) r2)
  | E (signe, e1), Q q1 -> Q (GrandRationnel_on.soustraire q1 (signe, e1, [1]))
  | Q q1, E (signe, e1) -> Q (GrandRationnel_on.soustraire q1 (signe, e1, [1]))
  | R r1, Q q1 -> Q (GrandRationnel_on.soustraire q1 (q_depuis_r r1))
  | Q q1, R r1 -> Q (GrandRationnel_on.soustraire q1 (q_depuis_r r1))

let soustraire e = soustraire e |> vers_grandentier

let additioner = function
    E e1, E e2 -> E (GrandEntier_on.additioner e1 e2)
  | R r1, R r2 -> R (GrandReel_on.additioner r1 r2)
  | Q q1, Q q2 -> Q (GrandRationnel_on.additioner q1 q2)
  | R r2, E (signe, e1) -> R (GrandReel_on.additioner (signe, e1, 0) r2)
  | E (signe, e1), R r2 -> R (GrandReel_on.additioner (signe, e1, 0) r2)
  | E (signe, e1), Q q1 -> Q (GrandRationnel_on.additioner q1 (signe, e1, [1]))
  | Q q1, E (signe, e1) -> Q (GrandRationnel_on.additioner q1 (signe, e1, [1]))
  | R r1, Q q1 -> Q (GrandRationnel_on.additioner q1 (q_depuis_r r1))
  | Q q1, R r1 -> Q (GrandRationnel_on.additioner q1 (q_depuis_r r1))

let additioner e = additioner e |> vers_grandentier

let diviser = function
    E (signe1, e1), E (signe2, e2) -> Q (signe1 <> signe2, e1, e2)
  | R r1, R r2 -> Q (GrandRationnel_on.diviser (q_depuis_r r1) (q_depuis_r r2))
  | Q q1, Q q2 -> Q (GrandRationnel_on.diviser q1 q2)
  | R r2, E (signe, e1) -> Q (GrandRationnel_on.diviser (signe, e1, [1]) (q_depuis_r r2))
  | E (signe, e1), R r2 -> Q (GrandRationnel_on.diviser (signe, e1, [1]) (q_depuis_r r2))
  | E (signe, e1), Q q1 -> Q (GrandRationnel_on.diviser q1 (signe, e1, [1]))
  | Q q1, E (signe, e1) -> Q (GrandRationnel_on.diviser q1 (signe, e1, [1]))
  | R r1, Q q1 -> Q (GrandRationnel_on.diviser q1 (q_depuis_r r1))
  | Q q1, R r1 -> Q (GrandRationnel_on.diviser q1 (q_depuis_r r1))

let diviser e = diviser e |> vers_grandentier

let opposer = function
    E (signe, e1) -> E (not signe, e1)
  | Q (signe, q1, q2) -> Q (not signe, q1, q2)
  | R (signe, r, exp) -> R (not signe, r, exp)

let opposer e = opposer e |> vers_grandentier

let inverser = function
    E (signe, e1) -> Q (signe, [1], e1)
  | Q (signe, q1, q2) -> Q (signe, q2, q1)
  | R r1 ->
    let a, b, c = q_depuis_r r1 in
    Q (a, c, b)

let inverser e = inverser e |> vers_grandentier

let texte_depuis_num = function
    E e1 ->  GrandEntier_on.texte_depuis_grandentier e1
  | R r1 -> GrandReel_on.texte_depuis_grandreel r1
  | Q (signe, q1, q2) ->
    let q1 = GrandEntier_on.texte_depuis_grandentier (false, q1)
    and q2 = GrandEntier_on.texte_depuis_grandentier (false, q2) in
    let q = q1 ^ "/" ^ q2 in
    if signe then
      "-" ^ q
    else
      q

let est_negatif = function
    E (signe, _) -> signe
  | Q (signe, _, _) -> signe
  | R (signe, _, _) -> signe
