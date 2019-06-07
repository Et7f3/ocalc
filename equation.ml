let lire_sys nom_fichier =
  let nom_fichier = open_in nom_fichier in
  let rec boucle acc =
    try
      let s = input_line nom_fichier in
      let s = String.split_on_char '\t' s in
      let s = Array.of_list s in
      boucle (s :: acc)
    with End_of_file ->
      let () = close_in nom_fichier in
      match List.rev acc with
        [] -> failwith "pas assez de ligne"
      | e :: l -> l |> Array.of_list, e
  in let mat, inc = boucle [] in
  let w = Array.length inc in
  let mat = Array.map (function ligne -> Array.map float_of_string ligne) mat in
  let h = Array.length mat in
  mat, w, h, inc

let addition_ligne ligne lignej =
  Array.mapi (fun i -> (+.) lignej.(i)) ligne

let multiplication_ligne ligne j =
  Array.mapi (fun _ e -> e *. j) ligne

let comb_lineaire mat (a, i) (b, j) =
  Array.mapi (fun k e -> a *. e +. b *. mat.(j).(k)) mat.(i)

let echanger_ligne mat i j =
  let l = mat.(i) in
  let () = mat.(i) <- mat.(j) in
  mat.(j) <- l

let ajouter_ligne mat i w h =
  let l = Array.make (w + 1) 0. in
  let () = l.(i) <- 1. in
  let () = l.(w) <- 1. in
  let () = mat := Array.append !mat [| l |] in
  incr h

(** check if zero for the float*)
let zero a =
  -0.0001 < a && a < 0.0001

(** print unkwon and coefficient*)
let print_mat inc mat =
  let () = Array.iter (Printf.printf "%.8s ") inc in
  let () = print_endline "" in
  Array.iter (fun l ->
    let () = Array.iter (Printf.printf "%.6f ") l in
    print_endline ""
  ) !mat

let triangle_superieur (* inc *) mat h w =
  let mat = ref mat
  and h = ref h in
  let () =
    for i = 0 to pred w do
      (*let () = Printf.printf "on traite la colonne i: %d\n" i in*)
      let () =
        if i > pred !h then
          (*let () = Printf.printf "il nous manque une ligne\n" in
          let () = *)ajouter_ligne mat i w h
          (*in print_mat inc mat*)
      in let () =
        if zero !mat.(i).(i) then
          (*let () = Printf.printf "notre pivot est nul\n" in*)
          let () =
            for k = i + 1 to pred !h do
              if not (zero !mat.(k).(i)) then
                echanger_ligne !mat i i
            done
          in(* let () = print_mat inc mat in*)
          if zero !mat.(i).(i) then
            (*let () = Printf.printf "on n'a pas de remplacant\n" in*)
            let () = ajouter_ligne mat i w h in
            (*let () = *)echanger_ligne !mat (pred !h) i(* in
            print_mat inc mat*)
      in for j = i + 1 to pred !h do
        (*let () = Printf.printf "on traite la ligne j: %d\n" j in
        let () = print_mat inc mat in
        let () =*)
          !mat.(j) <- comb_lineaire !mat (!mat.(j).(i), i) (~-. (!mat.(i).(i)), j)
        (*in print_mat inc mat*)
      done
    done
  in !mat, !h

let nomalise (* inc *) mat h w =
  let () =
    for i = 0 to pred w do
      (*let () =
        Printf.printf "On reduit la ligne i: %d selon le pivot (%d, %d): %f\n"
        i i i mat.(i).(i)
      in let () = *)mat.(i) <- multiplication_ligne mat.(i) (1. /. mat.(i).(i))(* in
      print_mat inc (ref mat)*)
    done
  in mat

let remonte (* inc *) mat w =
  let () =
    for i = pred w downto 0 do
      for k = i - 1 downto 0 do
        (*let () = Printf.printf "i: %d, k: %d, val: %f, val: %f\n" i k (mat.(i).(i)) (mat.(k).(i)) in
        let () = *)mat.(k) <- comb_lineaire mat (~-.(mat.(k).(i)), i) (mat.(i).(i), k)(* in
        print_mat inc (ref mat)*)
      done
    done
  in mat

let contractition_presente mat w h =
  let resultat = ref false in
  let () =
    for i = w to pred h do
      (*let () = Printf.printf "i: %d, w: %d\n" i w in*)
      resultat := not (Float.classify_float mat.(i).(w) = Float.FP_zero) || !resultat
    done
  in !resultat

let sauvegarder_resultat mat w h inc fichier =
  let fichier = open_out fichier in
  let () =
    if contractition_presente mat w h then
      output_string fichier "il y a une contractition presente\n"
    else
      Array.iteri (fun i e -> Printf.fprintf fichier "%s = %f\n" e mat.(i).(w)) inc in
  close_out fichier

let resoudre_sys mat w h inc =
  let mat, h = triangle_superieur (* inc *) mat h w in
  let mat = nomalise (* inc *) mat h w in
  let mat = remonte (* inc *) mat w in
  mat, w, h, inc;;

let resoudre_sys_depuis_fichier nom_fichier sortie_fichier =
  let mat, w, h, inc = lire_sys nom_fichier in
  (*let () = Printf.printf "On a lu le systeme et on a w: %d, h: %d\n" w h in*)
  let mat, w, h, inc = resoudre_sys mat w h inc in
  sauvegarder_resultat mat w h inc sortie_fichier

(** examples files*)
let res = resoudre_sys_depuis_fichier "equation0.equ" "solution0.res";;
let res = resoudre_sys_depuis_fichier "equation1.equ" "solution1.res";;
let res = resoudre_sys_depuis_fichier "equation2.equ" "solution2.res";;
let res = resoudre_sys_depuis_fichier "equation3.equ" "solution3.res";;
let res = resoudre_sys_depuis_fichier "equation4.equ" "solution4.res";;
let res = resoudre_sys_depuis_fichier "equation5.equ" "solution5.res";;
let res = resoudre_sys_depuis_fichier "equation6.equ" "solution6.res";;
