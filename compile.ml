let faire_dossier name =
  if Sys.file_exists name then
    if not (Sys.is_directory name) then
      failwith (name ^ " existe déjà et n'est pas un dossier")
  else
    Unix.mkdir name 777

let faire_dossiers =
  let rec faire_un_dossier = function
      [] -> ()
    | e :: l -> faire_dossier e; Unix.chdir e; faire_un_dossier l; Unix.chdir ".."
  in let rec faire_dossiers = function
    [] -> ()
  | e :: l -> faire_un_dossier e; faire_dossiers l
  in faire_dossiers

let gestionnaire_tester i argc = fun _ -> ()

let gestionnaire_construire i = gestionnaire_tester i

let parser_arg arg =
  let len = Array.length arg in
  let rec l = function
      i when i = len -> ()
    | _ -> ()
  in l 0
let () =
  let rec pr i = function
      arg when i = Array.length arg -> ()
    | arg -> print_endline arg.(i); pr (i +1) arg
  in pr 1 Sys.argv
