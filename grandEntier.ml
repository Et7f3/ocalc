type grandentier = bool * int list
(** grandentier est un tuple [(signe négatif, \[unité, dizaine, centaine, ...\])]

[-123] correspond à [(true, \[3; 2; 1\])] *)

let signe = function x -> false
(** renvoie [grandentier < 0] *)

let comparer ga gb = 0
(** 1 si ga < gb sinon 0 si ga = gb sinon -1 *)

let additionner ga gb = (false, [])
(** renvoie ga + gb *)

let soustraire ga gb = (false, [])
(** renvoie ga - gb *)

let multiplier ga gb = (false, [])
(** renvoie ga * gb *)

let pgcd ga gb = (false, [])
(** renvoie pgcd(ga, gb) *)

let diviser_multiple ga gb = (false, [])
(** renvoie ga / gb où ga est multiple de gb *)

let diviser ga gb = ((false, []), (false, []))
(** renvoie (nominateur, dénominateur) de la fraction ga / gb *)

let grandentier_depuis_texte sa = (false, [])
(** renvoie le grandentier à partir de sa représentation textuelle *)

let texte_depuis_grandentier ga = ""
(** renvoie la représentation textuelle d'un grandentier *)


let abs n =
	if n < 0
	then -n
	else n;;


let rec length = function
    [] -> 0
  | _::l -> 1 + length l ;;


let convert grandentier =
	let a = abs grandentier
in
let rec convert_rec  a = match a with
| a when (a < 10) -> a :: []
| _ -> (a mod 10) :: convert_rec(a / 10)
in
convert_rec a;;


let sign grandentier =  if grandentier > 0
then false
else true;;

(*
type grandentier = (sign grandentier,convert grandentier);;
*)
(** grandentier est un tuple [(signe négatif, \[unité, dizaine, centaine, ...\])]
[-123] correspond à [(true, \[3; 2; 1\])] *)

(*let signe = function x -> false*)
(** renvoie [grandentier < 0] *);;

let rec search_pos x = function
    [] -> failwith "search_pos: not found"
  | e :: l -> (if e = x then 0 else 1) + search_pos x l ;;

let nth n l =
  if n < 0 then
    invalid_arg "nth: index must be a natural"
  else
    let rec nth_rec = function
      | ([], _) -> failwith "nth: list is too short"
      | (e::_, 0) -> e
      | (_::l, n) -> nth_rec (l, n-1)
    in
      nth_rec (l, n) ;;

let comparer_nbr_pos ga gb =
	let rec cnbr i = function
|(ga,gb) when length ga > length gb -> -1
|(ga,gb) when length ga < length gb -> 1
|_ -> if i = (length ga) then 0
else if (nth ((length ga) - i) (* of what *)) < (nth ((length gb) - i) (* of what *)) then 1
else if (nth ((length ga) - i) (* of what *)) > (nth ((length gb) - i) (* of what *))  then -1
else cnbr (i+1) (ga,gb)
in
cnbr 1 (ga,gb);;

(*let comparer_nbr_neg ga gb =
	let rec cnbr ga gb i = function
|(ga,gb) when length ga < length gb -> -1
|(ga,gb) when length ga > length gb -> 1
|_ -> if i = length then 0
else if (nth (length ga - i)) > (nth length (gb - i)) then 1
else if (nth (length ga - i)) < (nth length (gb - i))  then -1
else cnbr ga gb (i+1)
in
cnbr ga gb 1;;
*)



let comparer ga gb = match ga, gb with
| (true,_),(false,_) -> 1
| (false,_),(true,_) -> (-1)
|((false,b),(false,d)) -> comparer_nbr_pos b d
|((true,b),(true,d)) -> - comparer_nbr_pos b d;;

(** 1 si ga < gb sinon 0 si ga = gb sinon -1 *)
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
    add (big1, big2) ;;

let soustraction a b = if comparer_nbr_pos a b = 1 then
let rec sous = function
      ([], r) | (r, []) -> r
    | (d1::r1, d2::r2) ->
        let s = d1 - d2 in
         if s > 0 then
           s :: sous(a,b)
       else
          (10 + d1 - d2) :: sous (r1,(bigint_sum [1] r2))
      in sous (a,b)
  else
  let rec sous = function
      ([], r) | (r, []) -> r
    | (d1::r1, d2::r2) ->
        let s = d1 - d2 in
         if s > 0 then
           s :: sous(a,b)
       else
          (10 + d1 - d2) :: sous (r1,(bigint_sum [1] r2))
      in sous (b,a);;


let bigint_mult big n =
  if n < 0 then
    invalid_arg "bigint_mult: negative multiplier"
  else
    let rec mult n = function
        ([], 0) -> []
      | ([], carry) -> (*bigint_of_int *) [carry]
      | (d::r, carry) -> let res = n * d + carry in
			 (res mod 10) :: mult n (r, res/10)
    in
    match n with
	     0 -> []
      | 1 -> big
      | n -> mult n (big, 0) ;;


let bigint_times big1 big2 =
  let rec mult = function
      ([], _) | (_, []) -> []
    | (0::r, big) | (big, 0::r) -> 0::mult (r, big)
    | (1::[], big) | (big, 1::[]) -> big
    | (1::r, big) | (big, 1::r) -> bigint_sum big (0::mult (r, big))
    | (d::r, big) -> bigint_sum (bigint_mult big d) (0::mult (r, big))
  in
    mult (big1, big2) ;;

let additionner ga gb = function
|((a,b),(c,d)) when a = c -> (a,bigint_sum b d)
(* et les patates ? *)


(** 1 si ga < gb sinon 0 si ga = gb sinon -1 *)
(** renvoie ga + gb *)

let soustraire ga gb = function
|((a,b),(c,d)) when a = false && c = true -> (false,bigint_sum b d)
|((a,b),(c,d)) when a = true && c = false -> (true,bigint_sum b d)
|((a,b),(c,d)) when a = c -> if (comparer_nbr_pos b d) = -1 (* il verifie pour le signe a renvoyer,ici ga > gb*)
							then (a,soustraction b d)
							else (not a, soustraction b d);;
(* et les patates ? *)

(** renvoie ga - gb *)

let multiplier ga gb = function
|((a,b),(c,d)) when a = c -> (false,bigint_times b d)
|((a,b),(c,d)) -> (true,bigint_times b d)
(** renvoie ga * gb *)

let pgcd ga gb = (false, [])
(** renvoie pgcd(ga, gb) *)

let diviser_multiple ga gb =
	let rec d_m ga gb d = if (comparer ga (true, (bigint_mult gb d))) =  1 then (d-1) else d_m ga gb (d+1) in d_m ga gb 1
(** renvoie ga / gb où ga est multiple de gb *)

(** 1 si ga < gb sinon 0 si ga = gb sinon -1 *)

(*
let diviser ga gb = let d = diviser_multiple ga gb and (a,b) =
et le frommage ... syntaxe error mec c'est pas complet
*)


(** renvoie (nominateur, dénominateur) de la fraction ga / gb *)

let grandentier_depuis_texte sa =
	let a = int_of_string sa
in
(sign a,convert a);;
(** renvoie le grandentier à partir de sa représentation textuelle *)

let textedechiffre ga =
	let rec tdc ga = match ga with
	| (_,[]) -> ""
	| (_,e::ga) -> e ^ tdc (false,ga)
	in
	tdc ga ;;
	(* mauvais prototype c'ets int list pas string list... *)
(*Convertit basiquement le nombre*)

let texte_depuis_grandentier ga =
	let (a,b) = ga in
	if a = true
	then "-" ^ textedechiffre ga
	else textedechiffre ga ;;

(** renvoie la représentation textuelle d'un grandentier *)
