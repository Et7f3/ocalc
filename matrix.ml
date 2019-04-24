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

module GenericMatrix (V : Value) = struct
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

module TestMatrix = GenericMatrix(Test)

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

let i3 = TestMatrix.identite 3
let res1 = TestMatrix.additioner m1 m2
let res2 = TestMatrix.soustraire res1 m2
let res3 = TestMatrix.multiplier i3 m1
(*
let () = TestMatrix.print i3
let () = TestMatrix.print m1
let () = TestMatrix.print res1
let () = TestMatrix.print res2
let () = TestMatrix.print res3
*)
