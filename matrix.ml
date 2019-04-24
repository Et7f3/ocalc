module type Value = sig
    type t

    val zero : t
    val unit : t
    val additioner : t -> t -> t
    val soustraire : t -> t -> t
    val diviser : t -> t -> t
    val multiplier : t -> t -> t
    val print : t -> unit
end

module Generic_matrix (V : Value) = struct
    type t = V.t array array

    let print = V.print
    let init n p = Array.make_matrix n p V.zero
    let protect m = Array.length m > 0 && Array.length m.(0) > 0
    let size m =
        let h = Array.length m in
        if h = 0 then
            h, 0
        else
            h, Array.length m.(0)
    let foreach ?(line = function _ -> ()) m f =
        if protect m then
            let h, w = size m in
            for i = 0 to pred h do
                let () =
                    for j = 0 to pred w do
                        f i j m.(i).(j)
                    done
                in line m.(i)
            done
    let print m =
        let () =
            foreach ~line:(fun _ -> print_char '\n') m (fun _ _ -> V.print)
        in print_char '\n'
    let foreach2 f m1 m2 =
        let (h, w) = size m1 in
        if (h, w) = size m2 then
            let mres = init h w in
            let () = foreach m1 (fun i j e -> mres.(i).(j) <- f e m2.(i).(j)) in
            mres
        else
            failwith "taille mauvaise dimension"
    let identite n =
        let mres = init n n in
        let () = foreach mres (fun i j _ -> mres.(i).(j) <- if i = j then V.unit else V.zero) in
        mres
    let additioner m1 m2 = foreach2 V.additioner m1 m2
    let soustraire m1 m2 = foreach2 V.soustraire m1 m2
    let multiplier m1 m2 =
        let (n, p) = size m1
        and (p', q) = size m2 in
        if p = p' then
            let mres = init n n in
            let prod_ligne_colonne i j =
                let res = ref V.zero in
                let () =
                    for k = 0 to pred p do
                        res := V.additioner !res (V.multiplier m1.(i).(k) m2.(k).(j))
                    done
                in !res
            in let () = foreach mres (fun i j _ -> mres.(i).(j) <- prod_ligne_colonne i j) in
            mres
        else
            failwith "taille mauvaise dimension"
    let multiplier_scalaire m scalaire =
        let (h, w) = size m in
        let mres = init h w in
        let () = foreach m (fun i j _ -> mres.(i).(j) <- V.multiplier m.(i).(j) scalaire) in
        mres
    let inverser m =
        let h, w = size m in
        let n = min h w in
        let mres = init w h in
        let () =
            for i = 0 to pred n do () done
        in mres

end

module Test = struct
    type t = int

    let zero = 0
    let unit = 1
    let additioner = ( + )
    let soustraire = ( - )
    let diviser = ( / )
    let multiplier = ( * )
    let print = Printf.printf "%5d"
end

module Test_matrix = Generic_matrix(Test)

module Test_float = struct
    type t = float

    let zero = 0.
    let unit = 1.
    let additioner = ( +. )
    let soustraire = ( -. )
    let diviser = ( /. )
    let multiplier = ( *. )
    let print = Printf.printf "%10.6f"
end

module Test_float_matrix = Generic_matrix(Test_float)

let m1 =
[|
    [|1; 2; 3|];
    [|4; 5; 6|];
    [|7; 8; 9|];
|]

let m2 =
[|
    [|9; 8; 7|];
    [|6; 5; 4|];
    [|3; 2; 1|];
|]

let m3 =
[|
    [|6.; 6.; 6.|];
    [|4.; 7.; 6.|];
    [|6.; 4.; 6.|];
|]

let inv_m3 =
[|
    [|9.  ; -6.; -3.|];
    [|6.  ;  0.; -6.|];
    [|-13.;  6.;  9.|];
|]

(*let i3 = Test_matrix.identite 3
let res1 = Test_matrix.additioner m1 m2
let res2 = Test_matrix.soustraire res1 m2
let res3 = Test_matrix.multiplier i3 m1*)
let res4 = Test_float_matrix.inverser m3
let () = Test_float_matrix.print (Test_float_matrix.multiplier res4 m3)
(*
let () = Test_matrix.print i3
let () = Test_matrix.print m1
let () = Test_matrix.print res1
let () = Test_matrix.print res2
let () = Test_matrix.print res3
*)
