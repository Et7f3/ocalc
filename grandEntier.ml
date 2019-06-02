type grandentier = bool * int list
(** grandentier est un tuple [(signe négatif, \[unité, dizaine, centaine, ...\])]
    [ - 123] correspond à [(true, \[3; 2; 1\])] *)

let zero = false, []

let unit = false, [1]

(** renvoie [grandentier < 0] *)
let est_negatif (signe, _) = signe

(** renvoie l'oposé *)
let neg (a, b) =
  match b with
    [] -> false, []
  | _ -> not a, b

let nettoyer_zero a =
  let rec n0 = function
      [] -> []
    | 0 :: c -> n0 c
    | c -> List.rev c
  in n0 (List.rev a)

(** 1 si ga < gb sinon 0 si ga = gb sinon -1 *)
(* 1 : gb > ga
   0 : ga = gb
  -1 : ga > gb*)

let comparer_nbr_abs ga gb =
  let la = List.length ga
  and lb = List.length gb in
  if la > lb then
    -1
  else if la < lb then
   1
  else
    let rec cna = function
        e1 :: ga, e2 :: gb ->
        if e1 > e2 then
          -1
        else if e2 > e1 then
          1
        else
          cna (ga, gb)
      (* this will not happen *)
      | [], [] -> 0
      | r, [] -> -1
      | [], r -> 1
    in cna (List.rev ga, List.rev gb)

let comparer ga gb =
  match ga, gb with
    (true, _), (false, _) -> 1
  | (false, _), (true, _) -> -1
  | (false, b), (false, d) | (true, d), (true, b) -> comparer_nbr_abs b d
(** 1 si ga < gb sinon 0 si ga = gb sinon -1 *)
(* 1 : gb > ga
   0 : ga = gb
   -1 : ga > gb*)

let bigint_sum big1 big2 =
  let rec add = function
      [], r | r, [] -> r
    | d1 :: r1, d2 :: r2 ->
      let s = d1 + d2 in
      if s < 10 then
        s :: add(r1, r2)
      else
        (s - 10) :: add (add([1], r1), r2)
  in add (big1, big2)

(* Constructeur propre a la sousatraction qui va
permettre de ne pas effectuer la retenue *)
let bigint_sum_for_sous big1 big2 =
  let rec add = function
      [], r | r, [] -> r
    | d1 :: r1, d2 :: r2 ->
      let s = d1 + d2 in
      s :: add (r1, r2)
  in add (big1, big2)

(* On preferera a > b en abs et on aura toujours*)
let rec sous = function
    [], [] -> []
  | [], r | r, [] -> r
  | d1 :: r1, d2 :: r2 ->
    if d1 >= d2 then
      (d1 - d2) :: sous(r1, r2)
    else
      (d1 + 10 - d2) :: sous (r1, (bigint_sum_for_sous [1] r2))

let sous a = sous a |> nettoyer_zero

(** renvoie ga + gb *)
let additionner ga gb =
  match ga, gb with
    (true, b), (false, d) | (false, d), (true, b) ->
      if comparer_nbr_abs b d = -1 then
        true, sous (b, d)
      else
        false, sous (d, b)
  | (a, b), (_ (* = a *), d) -> a, bigint_sum b d

(** renvoie ga - gb *)
let soustraire ga gb =
  match ga, gb with
    (true, b), (true, d) | (false, d), (false, b) ->
    if comparer_nbr_abs b d = -1 then
      true, sous (b, d)
    else
      false, sous (d, b)
  | (a, b), (_ (* not a *), d) -> a, bigint_sum b d


let bigint_mult big n =
  if n < 0 then
    invalid_arg "bigint_mult: negative multiplier"
  else
    let rec mult n = function
        [], 0 -> []
      | [], carry -> (* bigint_of_int *) [carry]
      | d :: r, carry ->
        let res = n * d + carry in
          (res mod 10) :: mult n (r, res / 10)
    in
    match n with
      0 -> []
    | 1 -> big
    | n -> mult n (big, 0)


let bigint_times big1 big2 =
  let rec mult = function
      [], _ | _, [] -> []
    | 0 :: r, big | big, 0 :: r -> 0 :: mult (r, big)
    | 1 :: [], big | big, 1 :: [] -> big
    | 1 :: r, big | big, 1 :: r -> bigint_sum big (0 :: mult (r, big))
    | d :: r, big -> bigint_sum (bigint_mult big d) (0 :: mult (r, big))
  in mult (big1, big2)

(** renvoie ga * gb *)
let multiplier ga gb =
  match ga, gb with
    (a, b), (c, d) when a = c -> false, bigint_times b d
  | (_, b), (_, d) -> true, bigint_times b d

(** renvoie pgcd(ga, gb) *)
let pgcd _ _ = (false, [])

let diviser_multiple_abs a b =
  let rec d_m a b e =
    let cmp = comparer_nbr_abs a (bigint_times b e) in
    if cmp = 0 then
      e
    else if cmp = 1 then
      sous (e, [1])
    else
      d_m a b (bigint_sum e [1])
  in d_m a b [1]

(** renvoie ga / gb où ga est multiple de gb *)
let diviser_multiple ga gb =
  match ga, gb with
    (_, _), (_, []) -> failwith "Nique ta mere"
  | (a, b), (c, d) -> not a = c, diviser_multiple_abs b d

let modulo ga gb =
  match ga, gb with
    (_, _), (_, []) -> failwith "Ah"
  | (_ (* a *), b), (_ (* c *), d) ->
    let rec modulo_rec i j =
      if comparer_nbr_abs i j = -1 then
        modulo_rec (sous(i, j)) j
      else
        i
    in modulo_rec b d

(** renvoie (nominateur, dénominateur) de la fraction ga / gb *)
let diviser _ _ = ((false, []), (false, []))

(** renvoie le grandentier à partir de sa représentation textuelle *)
let cse_rec a n =
  let rec abc a i =
    match i with
      i when i = n - 1 -> []
    | _ -> (int_of_char a.[i]) - 48 :: abc a (i - 1)
  in abc a (String.length a - 1) |> nettoyer_zero

let grandentier_depuis_texte sa =
  if sa.[0] = '-' then
    true, cse_rec sa 1
  else if sa.[0] = '+' then
    false, cse_rec sa 1
  else
    false, cse_rec sa 0


(*Convertit basiquement le nombre*)
let textedechiffre ga =
  let rec tdc acc = function
      [] -> acc
    | e :: ga -> (tdc [@tailcall]) ((string_of_int e) ^ acc) ga
  in tdc "" ga

(** renvoie la représentation textuelle d'un grandentier *)
let texte_depuis_grandentier ga =
  let a, b = ga in
  if a then
    "-" ^ textedechiffre b
  else
    textedechiffre b
