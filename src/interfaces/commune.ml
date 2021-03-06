let argv = Sys.argv
let argc = Array.length argv
let api_url = "https://api.github.com/repos/Et7f3/ocalc/releases"

let liste_historique = ref []

let evaluer_fichier fichier contexte =
  if Revery.Environment.webGL then
    contexte
  else
    if Filename.check_suffix fichier ".math" then
      try
        let fichier = open_in fichier in
        let rec boucle f =
          try
            let entree = input_line fichier in
            let sortie, f =
              try
                Noyau.Moteur.evaluate_with_history entree f
              with Division_by_zero -> (I18n.division_par_zero ()), f
            in let hist = Printf.sprintf "%s = %s" entree sortie in
            let () = liste_historique := hist :: !liste_historique in
            let () = print_endline hist in
            let () = flush stdout in
            boucle f
          with End_of_file ->
            let () = close_in fichier in
            f
        in boucle contexte
      with Sys_error _ ->
        let () = prerr_endline (I18n.fichier_inexistant fichier) in
        contexte
    else
      let () = prerr_endline (I18n.fichier_pas_math fichier) in
      contexte

let init_context =
  if Revery.Environment.webGL then
    Noyau.Moteur.empty_context
  else
    let evaluate_arg f max =
      let rec evaluate_arg f i =
        if i = max then
          let () = flush stdout in
          let () = flush stderr in
          f
        else
          let f = evaluer_fichier Sys.argv.(i) f in
          evaluate_arg f (i + 1)
        in evaluate_arg f 1
    in evaluate_arg Noyau.Moteur.empty_context (Array.length Sys.argv)

(*
let ip = Unix.((gethostbyname "localhost").h_addr_list.(0))
let addr = Unix.ADDR_INET (ip, 80)

let s =
    "GET /repos/Et7f3/ocalc/releases HTTP/1.1\r\n\
    Host: api.github.com\r\n\
    User-Agent: OCaml\r\n\
    Connection: close\r\n\
    \r\n"

let sock = Unix.(socket PF_INET SOCK_STREAM 0)
let _ = Unix.connect sock addr

let in_ch = Unix.in_channel_of_descr sock
let out_ch = Unix.out_channel_of_descr sock



let _ =
  output_string out_ch
s;
  flush out_ch



let g =
  try
    let () = while input_line in_ch <> "\r"; do () done in
    Yojson.Basic.from_channel in_ch
  with End_of_file ->
    let () = Unix.close sock in
    Yojson.Basic.from_string "[]";;

match g with
    `List l -> List.iter (function g -> Printf.printf "%s\n\n" (Yojson.Basic.to_string g)) l
    | _ -> print_endline "can't load versions"
*)
