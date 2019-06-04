let argv = Sys.argv
let argc = Array.length argv
let api_url = "https://api.github.com/repos/Et7f3/ocalc/releases"

let init_context =
  if Revery.Environment.webGL then
    Noyau.Moteur.empty_context
  else
    let evaluate_file f = f in
    evaluate_file Noyau.Moteur.empty_context

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
    | _ -> print_endline "can't laod versions"
*)
