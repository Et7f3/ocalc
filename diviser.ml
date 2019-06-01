let diviser_multiple_abs a b =
  let rec d_m a b e =
    let cmp = comparer_nbr_abs a (bigint_times b e) in
    if cmp = 0 then
      e
    else if cmp = 1 then
      sous (e,[1])
    else
      d_m a b (bigint_sum e [1])
  in d_m a b [1];;


let diviser_multiple_abs a b =
  let abaisser a e ret =
    let rec abaisser a n =
      let cmp = comparer_nbr_abs a e in
      if cmp = 1 then
        a, n
      else
        abaisser (sous (a, e)) (n + 1)
    in
    let a, n = abaisser a 0 in
    a, n :: ret
  in
  let rec div_mul a e =
    let cmp = comparer_nbr_abs a e in
    if cmp = 1 then
      a, []
    else
      let a, n = div_mul a (0 :: e) in
      abaisser a e n
  in let modulo, _ = div_mul a b in modulo;;


let diviser_multiple_abs a b =
  let abaisser a e ret =
    let rec abaisser a n =
      let cmp = comparer_nbr_abs a e in
      if cmp = 1 then
        a, n
      else
        abaisser (sous (a, e)) (n + 1)
    in
    let a, n = abaisser a 0 in
    a, n :: ret
  in
  let rec div_mul a e =
    let cmp = comparer_nbr_abs a e in
    if cmp = 1 then
      a, []
    else
      let a, n = div_mul a (0 :: e) in
      abaisser a e n
  in let _, e = div_mul a b in e;;

diviser_multiple_abs [1; 2;] [2];;

diviser_multiple_abs [1; 2; 3; 4; 5; 6; 2;] [2];;
