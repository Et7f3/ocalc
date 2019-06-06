module type Value = sig
    type t

    val zero : t
    val unit : t
    val neg : t -> t
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
    let __protect m = Array.length m > 0 && Array.length m.(0) > 0
    let size m =
        let h = Array.length m in
        if h = 0 then
            h, 0
        else
            h, Array.length m.(0)
    let foreach ?(line = function _ -> ()) m f =
        if __protect m then
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
            failwith (I18n.matrice_mauvaise_dimension ())
    let __vide_vers_identite m =
        let h, w = size m in
        let () =
            for i = 0 to pred (min h w) do
                m.(i).(i) <- V.unit
            done
        in m
    let identite n =
        let mres = init n n in
        __vide_vers_identite mres
    let additioner m1 m2 = foreach2 V.additioner m1 m2
    let soustraire m1 m2 = foreach2 V.soustraire m1 m2
    let multiplier m1 m2 =
        let (n, p) = size m1
        and (p', q) = size m2 in
        if p = p' then
            let mres = init n q in
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
        let () = foreach m (fun i j e -> mres.(i).(j) <- V.multiplier e scalaire) in
        mres
    module Operation_elementaires = struct
        let multiplier_ligne l scalaire =
            Array.map (V.multiplier scalaire) l
        let additioner_ligne l1 l2 =
            Array.mapi (fun k -> V.additioner l2.(k)) l1
        let neg_ligne l =
            Array.map V.neg l
        let echanger_ligne m i j =
            let tmp = m.(i) in
            let () = m.(i) <- m.(j) in
            m.(j) <- tmp
        let affecter_ligne m i l =
            m.(i) <- l
    end
    let inverser m' =
        let h, w = size m' in
        let m = init h w in
        let () = foreach m' (fun i j e -> m.(i).(j) <- e) in
        let n = min h w in
        let mres = init w h in
        let mres = __vide_vers_identite mres in
        let open Operation_elementaires in
        let () =
            for i = 0 to pred (pred n) do
                for j = i + 1 to pred h do
                    if m.(j).(i) <> V.zero then
                        let () =
                            if m.(i).(i) = V.zero then
                                let () = echanger_ligne m j i in
                                echanger_ligne mres j i
                            else
                                let pivot = m.(i).(i) in
                                let coeff = V.diviser m.(j).(i) pivot in
                                let modifier_ligne m =
                                    let ligne = multiplier_ligne m.(i) coeff in
                                    let ligne = neg_ligne ligne in
                                    let () = Printf.printf "%d, %d\n" (Array.length m.(j)) (Array.length ligne) in
                                    let ligne = additioner_ligne m.(j) ligne in
                                    affecter_ligne m j ligne
                                in let () = modifier_ligne m in
                                if j < n then
                                    modifier_ligne mres
                        (* in fact we should empty it *)
                        in print mres
                done
            done
        in let () = print m in
        mres

end

module Test_int = struct
    type t = int

    let zero = 0
    let unit = 1
    let neg = ( ~- )
    let additioner = ( + )
    let soustraire = ( - )
    let diviser = ( / )
    let multiplier = ( * )
    let print = Printf.printf "%5d"
end

module Test_int_matrix = Generic_matrix(Test_int)

module Test_float = struct
    type t = float

    let zero = 0.
    let unit = 1.
    let neg = ( ~-. )
    let additioner = ( +. )
    let soustraire = ( -. )
    let diviser = ( /. )
    let multiplier = ( *. )
    let print = Printf.printf "%10.6f"
end

module Test_float_matrix = Generic_matrix(Test_float)
