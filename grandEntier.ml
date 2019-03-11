type grandentier = bool * int list
(** grandentier est un tuple [(signe négatif, \[unité, dizaine, centaine, ...\])]
    [-123] correspond à [(true, \[3; 2; 1\])] *)

let est_negatif (signe, _) = signe
(** renvoie [grandentier < 0] *)


let rec length = function
    [] -> 0
  | _ :: l -> 1 + length l

let nth n l =
  if n < 0 then
    invalid_arg "nth: index must be a natural"
  else
    let rec nth_rec = function
        [], _ -> failwith "nth: list is too short"
      | e :: _, 0 -> e
      | _ :: l, n -> nth_rec (l, n-1)
    in nth_rec (l, n)

let comparer_nbr_abs ga gb =
  let rec cna i = function
      ga, gb when length ga > length gb -> -1
    | ga, gb when length ga < length gb -> 1
    | _ ->
      if i = (length ga) then
        0
      else if (nth ((length ga) - i) ga < (nth ((length gb) - i) gb)) then
        1
      else if (nth ((length ga) - i) ga > (nth ((length gb) - i) gb))  then
        -1
      else
        cna (i+1) (ga, gb)
  in cna 1 (ga, gb)

let comparer ga gb =
  match ga, gb with
    (true, _), (false, _) -> 1
  | (false, _), (true, _) -> (-1)
  |((false, b), (false, d)) -> comparer_nbr_abs b d
  |((true, d), (true, b)) -> comparer_nbr_abs b d
(** 1 si ga < gb sinon 0 si ga = gb sinon -1 *)
(* 1 : gb > ga
   0 : ga = gb
   -1 : ga > gb*)


let bigint_sum big1 big2 =
  let rec add = function
      ([], r) | (r, []) -> r
    | (d1::r1, d2::r2) ->
      let s = d1 + d2 in
      if s < 10 then
        s :: add(r1, r2)
      else
        (s - 10) :: add (add([1], r1), r2)
  in
  add (big1, big2)

let rec sous (a, b) =
  match (a, b) with (* On preferera a>b en abs*)
    ([], r) | (r, []) -> r
  | (d1::r1, d2::r2) ->
    let s = d1 - d2 in
    if s > 0 then
      s :: sous(a, b)
    else
      (10 + s) :: sous (r1, (bigint_sum [1] r2))

let additionner ga gb =
  match (ga, gb) with
    (true as a, b), (true, d) | (false as a, b), (false, d) -> (a, bigint_sum b d)
  | (true, b), (false, d) ->
    if comparer_nbr_abs b d = -1 then
      (true, sous (b, d))
    else
      (false, sous (b, d))
  | (false, b), (true, d) ->
    if comparer_nbr_abs b d = -1 then
      (false, sous (d, b))
    else
      (true, sous (b, d))
(** renvoie ga + gb *)

let soustraire ga gb =
  match (ga, gb) with
    (true as a, b), (false, d) | (false as a, b), (true, d) -> (a, bigint_sum b d)
  | (true, b), (true, d) ->
    if comparer_nbr_abs b d = -1 then
      true, sous (b, d)
    else
      false, sous (d, b)
  | (false, b), (false, d) ->
    if comparer_nbr_abs b d = -1 then
      false, sous (b, d)
    else
      true, sous (d, b)
(** renvoie ga - gb *)


let bigint_mult big n =
  if n < 0 then
    invalid_arg "bigint_mult: negative multiplier"
  else
    let rec mult n = function
        [], 0 -> []
      | [], carry -> (*bigint_of_int *) [carry]
      | d :: r, carry ->
        let res = n * d + carry in
        (res mod 10) :: mult n (r, res/10)
    in match n with
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

let multiplier ga gb =
  match ga, gb with
    (a, b), (c, d) when a = c -> false, bigint_times b d
  | (a, b), (c, d) -> true, bigint_times b d
(** renvoie ga * gb *)

let pgcd ga gb = (false, [])
(** renvoie pgcd(ga, gb) *)


let diviser_multiple_abs a b =
  let rec d_m a b e =
    if comparer_nbr_abs a (bigint_mult b e) =  1 then
      e - 1
    else
      d_m a b (e + 1)
  in d_m a b 1

let diviser_multiple ga gb =
  match (ga, gb) with
    (a, b), (c, d) when a = c -> false, diviser_multiple_abs b d
  | (a, b), (c, d) -> true, diviser_multiple_abs b d
(** renvoie ga / gb où ga est multiple de gb *)

let diviser ga gb = ((false, []), (false, []))
(** renvoie (nominateur, dénominateur) de la fraction ga / gb *)
(*
let diviser ga gb =
  match (ga, gb) with
  |
  |
  |
  |
*)


let cse_rec a n =
  let rec abc = function
      i when i = n - 1 -> []
    | i -> (int_of_char a.[i]) - 48 :: abc (i - 1)
  in abc (String.length a - 1)

let grandentier_depuis_texte sa =
  if sa.[0] = '-' then
    true, cse_rec sa 1
  else if sa.[0] = '+' then
    false, cse_rec sa 1
  else
    false, cse_rec sa 0
(** renvoie le grandentier à partir de sa représentation textuelle *)
(* let ga = grandentier_depuis_texte "-47436987436984376893768327692" *)


let textedechiffre ga =
  let rec tdc = function
      [] -> ""
    | e :: ga -> tdc ga ^ (string_of_int e)
  in tdc ga
(* Convertit basiquement le nombre *)

let texte_depuis_grandentier ga =
  let (a, b) = ga in
  if a then
    "-" ^ textedechiffre b
  else
    textedechiffre b
(** renvoie la représentation textuelle d'un grandentier *)
