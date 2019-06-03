

(** grandentier est un tuple [(signe négatif, \[unité, dizaine, centaine, ...\], exposant base 10)]
[-1,23] correspond à [(true, \[3; 2; 1\], -2)] *)
type grandreel = bool * int list * int

(** zero de grandreel *)
let zero = false, [], 0

(** unité de grandreel *)
let unit = false, [1], 0

(** renvoie [grandreel < 0] *)
let est_negatif (s, _, _) = s

(** renvoie l'oposé *)
let neg = function
    (_, [], _) -> false, [], 0
  | (s, m, e) -> not s, m, e

  
let remove a = 
 let rec r0 c = match c with 
      [] -> []
    | e :: c -> 
      if e = 0 then 
        r0 c
      else 
        List.rev (e :: c) 
  in r0 (List.rev a) 


let powerup a =  
  let rec cleaner = function
      (_,[],_) -> a
    | (s,e :: b,c) -> 
        if e = 0 then 
          cleaner (s, b, c+1)
        else
          (s,e :: b,c)
  in cleaner a


(*
Fonction Auxiliaire qui va mettre les deux valeurs sur la meme puissance
On choisira toujours la plus grande
*)
let reunir_puissance ga gb =
  let rec r_p ga gb =
    let (sa, va, a) = ga
    and (sb, vb, b) = gb in
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
  comparer (a,b) (d,e)

(** renvoie ga + gb *)
let additionner ga gb =
  let (a, b, c), (d, e, _) = reunir_puissance ga gb in
  let a, b = additionner (a,b) (d,e) in
  a,remove b, c

(** renvoie ga - gb *)
let soustraire ga gb =
  let (a, b, c), (d, e, _) = reunir_puissance ga gb in
  let a, b = soustraire (a,b) (d,e) in
  a,remove b, c

(** renvoie ga * gb *)
let multiplier (a, b, c) (d, e, f) =
  let a, b = multiplier (a, b) (d, e) in
  a, b, c + f

(** renvoie ga / gb *)
let diviser (a, b, c) (d, e, f) =
  let () =
    if e = [] then (* / 0 *)
      failwith "Nique ta gentil maman"
  in let signe = b <> [] (* 0 / a -> + *) && a <> d in
  let b = [] in
  signe, b, c - f

let grandReel_depuis_texte_transfo ga start = 
  let rec grdt e max cpt = match e with 
      (a,b,c) when cpt = max -> (a,remove b,c)
    | (a,b,c) when a -> grdt  (a, (int_of_char ga.[cpt] - 48) :: b , c - 1) max (cpt + 1)   
    | (a,b,c) -> 
    if ga.[cpt] = ',' || ga.[cpt] = '.' then
      grdt (true, b, c) max (cpt + 1)
    else
      grdt (a, (int_of_char ga.[cpt] - 48) :: b, c) max (cpt + 1)
  in grdt (false, [], 0) (String.length ga) start 


let grandreel_depuis_texte sa =
  if sa.[0] = '-' then
    let (_,a,b) = grandReel_depuis_texte_transfo sa 1 in 
    true, a , b
  else if sa.[0] = '+' then
    let (_,a,b) = grandReel_depuis_texte_transfo sa 1 in 
    false, a , b
  else
    let (_,a,b) = grandReel_depuis_texte_transfo sa 0 in 
    false, remove a , b

(*Convertit basiquement le nombre*)
let textedechiffre ga =
  let rec tdc = function
      [] -> ""
    | e :: ga -> tdc ga ^ (string_of_int e)
  in tdc ga

(** renvoie la représentation textuelle d'un grandentier *)
let texte_depuis_grandentier ga =
  let (a, b) = ga in
  if a = true then
    "-" ^ textedechiffre b
  else
    textedechiffre b

let rec ajouterdes0  texte nbr = 
  if nbr = 0 then
    texte
  else
    ajouterdes0 (texte ^ "0") (nbr - 1);;

let rec tdgcs a b = match (a,b) with
      ([],b) -> ""
    | (e :: a,b) when b = 0 -> tdgcs  (e :: a) (b + 1) ^ "," 
    | (e :: a,b) -> tdgcs a (b + 1) ^string_of_int e

let texte_depuis_grandreel_cas_neg (a, b, c) = 
  if a then 
    "-" ^ tdgcs b c
  else
    tdgcs b c




let texte_depuis_grandreel = function
| (false, [], 0) -> "0"
| (a, b, 0) -> texte_depuis_grandentier (a, b)
| (a, b, c) when c > 0 -> ajouterdes0 (texte_depuis_grandentier (a, b)) c
| ga -> texte_depuis_grandreel_cas_neg ga 

(*
  let t_depart = "42,24"
let ga = grandreel_depuis_texte t_depart
let resultat_correct = texte_depuis_grandreel ga = t_depart
*)
