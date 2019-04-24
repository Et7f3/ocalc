let is_neg ga = match ga with
| (true,_) -> true
| _ -> false ;;
(** renvoie [grandentier < 0] *)

(*let comparer ga gb = 0*)

let rec length = function
    [] -> 0
  | _::l -> 1 + length l ;;


let nth n l = (* Prend un element dans une liste *)
  if n < 0 then
    invalid_arg "nth: index must be a natural"
  else
    let rec nth_rec = function
      | ([], _) -> failwith "nth: list is too short"
      | (e::_, 0) -> e
      | (_::l, n) -> nth_rec (l, n-1)
    in
      nth_rec (l, n) ;;

(** 1 si ga < gb sinon 0 si ga = gb sinon -1 *)
(** 1 si ga < gb sinon 0 si ga = gb sinon -1 *)
(* 1 : gb > ga
   0 : ga = gb
  -1 : ga > gb*)
let comparer_nbr_abs ga gb =
  let la = length ga
  and lb  = length gb in
  (* Compare un a un les elements en partant de la fin *)
  let rec cna i = function
      ga, gb when la > lb -> -1
    | ga, gb when la < lb -> 1
    | _ ->
      if i = la then
        0
      else if nth (la - i) ga < (nth (lb - i) gb) then
        1
      else if nth (la - i) ga > (nth (lb - i) gb) then
        -1
      else
        cna (i+1) (ga, gb)
  in cna 1 (ga, gb)

let comparer ga gb = match ga, gb with
| (true,_),(false,_) -> 1
| (false,_),(true,_) -> (-1)
|((false,b),(false,d)) -> comparer_nbr_abs b d
|((true,b),(true,d)) -> - comparer_nbr_abs b d;;

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

let bigint_sum_for_sous big1 big2 = (* Constructeur propre a la sousatraction qui va permettre de ne pas effectuer la retenue*)
  let rec add = function
      ([], r) | (r, []) -> r
    | (d1::r1, d2::r2) ->
        let s = d1 + d2 in
            s :: add(r1, r2)
  in
    add (big1, big2) ;;

let rec sous (a,b) = match (a,b) with (* On preferera a>b en abs et on aura toujours*)
    | ([],[]) -> []
    | ([], r) | (r, []) -> r
    | (d1::r1, d2::r2) ->
         if d1 >= d2 then
           (d1-d2) :: sous(r1,r2)
       else
          (d1 + 10 - d2) :: sous (r1,(bigint_sum_for_sous [1] r2));;


(** 1 si ga < gb sinon 0 si ga = gb sinon -1 *)
(*
1  : gb > ga
0  : ga = gb
-1 : ga > gb
avec fonction ga gb
*)

let additionner ga gb = match (ga,gb) with
| ((true,b),(false,d)) ->
	if comparer_nbr_abs b d = -1 then
		(true,sous (b,d))
	else
		(false,sous (d,b))
| ((false,b),(true,d)) ->
	if comparer_nbr_abs b d = -1 then
		(false,sous (b,d))
	else
		(true,sous (d,b))
| ((a,b),(c,d)) -> (a,bigint_sum b d);;

(** renvoie ga + gb *)
(** 1 si ga < gb sinon 0 si ga = gb sinon -1 *)
(* 1 : gb > ga
   0 : ga = gb
  -1 : ga > gb*)


let soustraire ga gb = match (ga,gb) with
| ((true ,b),(false,d)) -> (true,bigint_sum b d)
| ((false,b),(true ,d)) -> (false,bigint_sum b d)
| ((true ,b),(true ,d)) ->
	if comparer_nbr_abs b d = -1 
	then
		(true,sous (b,d))
	else
		(false,sous (d,b))
| ((false,b),(false ,d)) ->
	if comparer_nbr_abs b d = -1 then
		(false,sous (b,d))
	else
		(true,sous (d,b));;



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

let multiplier ga gb = match ga,gb with
|((a,b),(c,d)) when a = c -> (false,bigint_times b d)
|((a,b),(c,d)) -> (true,bigint_times b d) ;;





(* 1 : gb > ga
   0 : ga = gb
  -1 : ga > gb*)

let diviser_multiple_abs a b =
	let rec d_m a b e =
	if (comparer_nbr_abs a (bigint_times b e)) >=  0 then
		e
	else
		d_m a b (bigint_sum e [1])
  in d_m a b [1] ;;

let diviser_multiple ga gb = match (ga,gb) with
| ((a,b),(c,d)) when d = [0] -> failwith("Nique ta mere")
| ((a,b),(c,d)) when a = c -> (false,diviser_multiple_abs b d)
| ((a,b),(c,d)) -> (true,diviser_multiple_abs b d);;

let modulo ga gb = match (ga,gb) with
| ((a,b),(c,[0])) -> failwith("Ah")
| ((a,b),(c,d)) ->
  let rec modulo_rec i j =
    if comparer_nbr_abs i j = -1
    then
      modulo_rec (sous(i,j)) j
    else
      i
  in
  modulo_rec b d;;


(** renvoie le grandentier à partir de sa représentation textuelle *)
let cse_rec a n =
	let rec abc a i = match i with
	| i when i = n-1 -> []
	| _ -> (int_of_char a.[i]) -48 :: abc a (i-1)
	in
	abc a (String.length a - 1);;

let grandentier_depuis_texte sa =
if sa.[0] = '-' then
	(true,cse_rec sa 1)
else if sa.[0] = '+' then
	(false,cse_rec sa 1)
else
	(false, cse_rec sa 0)


let texte_depuis_grandentier ga = ""
(** renvoie la représentation textuelle d'un grandentier *)

let textedechiffre ga =
	let rec tdc = function
	    [] -> ""
	  | e :: ga -> tdc ga ^ (string_of_int e)
	in tdc ga;;
	(* mauvais prototype c'ets int list pas string list... *)
(*Convertit basiquement le nombre*)

let texte_depuis_grandentier ga =
	let (a,b) = ga in
	if a = true
	then "-" ^ textedechiffre b
	else textedechiffre b;;

(** renvoie la représentation textuelle d'un grandentier *)
