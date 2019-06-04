(** grandentier est un tuple [(signe négatif, \[unité, dizaine, centaine, ...\], exposant base 10)]
[-1,23] correspond à [(true, \[3; 2; 1\], -2)] *)
type grandreel = bool * int list * int

(** zero de grandreel *)
let zero = false, [], 0

(** unité de grandreel *)
let unit = false, [1], 0

(** renvoie [grandreel < 0] *)
let est_negatif (s, _, _) = s

let is_nul (a,b,c) = b = []
(** renvoie l'oposé *)
let neg = function
    _, [], _ -> false, [], 0
  | s, m, e -> not s, m, e


let remove a =
  let rec r0 = function
      [] -> []
    | 0 :: c -> r0 c
    | c -> List.rev c
  in r0 (List.rev a)


let rec powerup = function
    _, [], _ -> false, [], 0
  | s, 0 :: b, c -> powerup (s, b, c + 1)
  | s, e :: b, c -> s, e :: b, c


(*
Fonction Auxiliaire qui va mettre les deux valeurs sur la meme puissance
On choisira toujours la plus grande
*)
let reunir_puissance ga gb =
  let rec r_p ga gb =
    let sa, va, a = ga
    and sb, vb, b = gb in
    if a = b then
      ga, gb
    else if a < b then
      r_p (sa, va, a) (sb, 0 :: vb, b - 1)
    else
      r_p (sa, 0 :: va, a - 1) (sb, vb, b)
  in r_p ga gb

  (** 1 si ga < gb sinon 0 si ga = gb sinon -1 *)
  (* 1 : gb > ga
  0 : ga = gb
  -1 : ga > gb*)

let comparer_gr ga gb =
  let (a, b, c), (d, e, _) = reunir_puissance ga gb in
  comparer (a, b) (d, e)

(** renvoie ga + gb *)
let additioner ga gb =
  let (a, b, c), (d, e, _) = reunir_puissance ga gb in
  let a, b = additioner (a, b) (d, e) in
  powerup (a, remove b, c)

(** renvoie ga - gb *)
let soustraire ga gb =
  let (a, b, c), (d, e, _) = reunir_puissance ga gb in
  let a, b = soustraire (a, b) (d, e) in
  powerup (a, remove b, c)

(** renvoie ga * gb *)
let multiplier (a, b, c) (d, e, f) =
  let a, b = multiplier (a, b) (d, e) in
  powerup (a, remove b, c + f)

(** renvoie ga / gb *)
let diviser (a, b, c) (d, e, f) =
  let () =
    if e = [] then (* / 0 *)
      failwith "Nique ta gentil maman"
  in let signe = b <> [] (* 0 / a -> + *) && a <> d in
  let b = [] in
  powerup (signe, remove b, c - f)

let grandReel_depuis_texte_transfo start ga =
  let max = String.length ga in
  let rec grdt (a, b, c) cpt =
    if cpt = max then
      b, c
    else
      let next =
        match a, ga.[cpt] with
          _, (',' | '.') -> true, b, c
        | true, cha -> true, (int_of_char cha - 48) :: b, c - 1
        | false, cha -> false, (int_of_char cha - 48) :: b, c
      in grdt next (cpt + 1)
  in grdt (false, [], 0) start


let grandreel_depuis_texte sa =
  let signe, transfo =
    match sa.[0] with (* sa is not "" *)
      '-' -> true, grandReel_depuis_texte_transfo 1
    | '+' -> false, grandReel_depuis_texte_transfo 1
    | _ -> false, grandReel_depuis_texte_transfo 0
  in let mantisse, exposant = transfo sa in
  (signe, remove mantisse, exposant) |> powerup

(*Convertit basiquement le nombre*)
let textedechiffre ga =
  let rec tdc = function
      [] -> ""
    | e :: ga -> tdc ga ^ (string_of_int e)
  in tdc ga

(** renvoie la représentation textuelle d'un grandentier *)
let texte_depuis_grandentier ga =
  let a, b = ga in
  if a then
    "-" ^ textedechiffre b
  else
    textedechiffre b

let rec ajouterdes0 texte nbr =
  if nbr = 0 then
    texte
  else
    ajouterdes0 (texte ^ "0") (nbr - 1)

let rec tdgcs = function
    [], b -> ""
  | a, 0 -> tdgcs (a, 1) ^ ","
  | e :: a, b -> tdgcs (a, b + 1) ^ string_of_int e

let texte_depuis_grandreel_cas_neg (a, b, c) =
  let texte = tdgcs (b, c) in
  if a then
    "-" ^ texte
  else
    texte

let texte_depuis_grandreel = function
    _, [], _ -> "0"
  | a, b, 0 -> texte_depuis_grandentier (a, b)
  | a, b, c when c > 0 -> ajouterdes0 (texte_depuis_grandentier (a, b)) c
  | ga -> texte_depuis_grandreel_cas_neg ga

(*
  let t_depart = "42,24"
let ga = grandreel_depuis_texte t_depart
let resultat_correct = texte_depuis_grandreel ga = t_depart
*)
