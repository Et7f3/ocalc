open Revery
open Revery.UI
open Revery.UI.Components


let bgColor =
  (*if Environment.webGL then
    Color.hex("#ffffff")
  else*)
    Color.hex("#212733")

let _ (* txtColor *) =
  (*if Environment.webGL then
    Color.rgb 0. 0. 0.
  else*)
    Color.rgb 255. 255. 255.

type vue =
  [
      `VueCalcul
    | `VueHistorique
    | `VueMatrice
    | `VueAccueil
    | `VueEquation
    | `VueBonus
  ]

(*
type vue_par_defaut_state =
{
  nothing: unit;
}
*)
type calcul_state =
  {
    valeur: string;
    res: string;
    liste_historique: string list;
    context: Noyau.Moteur.context;
  }

type historique_state =
  {
    mutable liste: string list;
  }

type matrice_mode =
  [
      `Solveur
    | `Addition
    | `Soustraction
    | `Multiplication
    | `Inverse
  ]

type matrice_state =
  {
    mode: matrice_mode;
    mutable matrice1: string array array;
    mutable matrice2: string array array;
    mutable matrice_res: string array array;
    mutable taille1: int * int;
    mutable taille2: int * int;
    mutable taille_res: int * int;
    message: string;
  }

type equation_state =
  {
    mutable inconnu: string list;
    mutable nbr_inc: int;
    mutable lines: int;
    mutable mat1: string array array;
    mutable mat2: string array array;
    res: string;
  }

type accueil_state =
  {
    lang : I18n.lang;
  }

type application_state =
  {
    vue_courante: vue;
  }

type application_sauvegarde =
  {
    calcul: calcul_state;
    equation: equation_state;
    historique: historique_state;
    matrice: matrice_state;
    accueil: accueil_state;
  }

(*
module VueParDefaut = struct
  let component = React.component "VueParDefaut"

  type action = Nope

  let reducer action etat =
    match action with
      Nope -> etat

  let createElement ~initialState ~changerVue ~onUpdate =
  fun ~children:_ () ->
    component
      (fun hooks ->
        let ({_} (* nouvel etat *) as etat, _ (* dispatch *), hooks) =
          React.Hooks.reducer ~initialState reducer hooks
        in let () = onUpdate (`VueParDefaut etat) in
        (hooks, View.createElement ~children:[] ()))
end
*)

module Bouton = struct
  let style_btn = Style.[fontSize 25; fontFamily "Roboto-Regular.ttf"]

  let menu_retour ~onMouseUp ?(style=[]) () =
    Text.createElement ~text:(I18n.retour ()) ~onMouseUp
      ~style:(style @ style_btn)
      ~children:[] ()

  let calculer ~onMouseUp ?(style=[]) () =
    Text.createElement ~text:(I18n.calculer ()) ~onMouseUp
      ~style:(style @ style_btn)
      ~children:[] ()

  let effacer ~onMouseUp ?(style=[]) () =
    Text.createElement ~text:(I18n.effacer ()) ~onMouseUp
      ~style:(style @ style_btn)
      ~children:[] ()

  let menu_historique ~onMouseUp ?(style=[]) () =
    Text.createElement ~text:(I18n.menu_historique ()) ~onMouseUp
      ~style:(style @ style_btn)
      ~children:[] ()

  let menu_matrices ~onMouseUp ?(style=[]) () =
    Text.createElement ~text:(I18n.menu_matrices ()) ~onMouseUp
      ~style:(style @ style_btn)
      ~children:[] ()

  let menu_accueil ~onMouseUp ?(style=[]) () =
    Text.createElement ~text:(I18n.menu_accueil ()) ~onMouseUp
      ~style:(style @ style_btn)
      ~children:[] ()

  let menu_calcul ~onMouseUp ?(style=[]) () =
    Text.createElement ~text:(I18n.menu_calcul ()) ~onMouseUp
      ~style:(style @ style_btn)
      ~children:[] ()

  let menu_equations ~onMouseUp ?(style=[]) () =
    Text.createElement ~text:(I18n.menu_equations ()) ~onMouseUp
      ~style:(style @ style_btn)
      ~children:[] ()

  let plus ~onMouseUp ?(style=[]) () =
    Text.createElement ~text:"+" ~onMouseUp
      ~style:(style @ style_btn)
      ~children:[] ()

  let moins ~onMouseUp ?(style=[]) () =
    Text.createElement ~text:"-" ~onMouseUp
      ~style:(style @ style_btn)
      ~children:[] ()
end

module VueBonus = struct
  let component = React.component "VueBonus"

  let createElement ~changerVue ~onUpdate:_ ~children:_ () =
    let retour_maison =
      Bouton.menu_retour ~onMouseUp:(fun _ -> changerVue `VueAccueil)
        ~style:Style.[position `Absolute; bottom 10; right 10] ()
    in Minesweeper_lib.MineSweeper.createElement ~children:[retour_maison] ()
end

module Calcul = struct
  let component = React.component "Calcul"

  type action =
      Vider
    | MiseAJour of string
    | Calculer

  let reducer action etat =
    match action with
      Vider -> {etat with valeur = ""}
    | MiseAJour valeur -> {etat with valeur} (* ici on met à jour notre état *)
    | Calculer ->
      let res = etat.valeur in
      let res, cxt =
        try
          Noyau.Moteur.evaluate_with_history res etat.context
        with
          Failure msg -> msg, etat.context (* old context *)
        | Division_by_zero ->
          I18n.division_par_zero (), etat.context (* old context *)
      in let res = etat.valeur ^ " = " ^ res in
      {
        liste_historique = res :: etat.liste_historique;
        context = cxt;
        valeur = "";
        res
      }

  let createElement ~initialState ~changerVue ~onUpdate =
    let containerStyle =
      Style.[
        position `Absolute;
        justifyContent `Center;
        alignItems `Center;
        bottom 0;
        top 0;
        left 0;
        right 0;
      ]
    in let textStyle =
      Style.[
        fontSize 25;
        fontFamily "Roboto-Regular.ttf";
        bottom 0;
        top 0;
        left 0;
        right 0;
      ]
    in fun ~children:_ () ->
      component (function hooks ->
        let ({valeur; res; _} as etat, dispatch, hooks) =
          React.Hooks.reducer ~initialState reducer hooks
        in let () = onUpdate (`Calcul etat) in
        let bouton_calc =
          Bouton.calculer ~style:[] ~onMouseUp:(fun _ -> dispatch Calculer) ()
        in let bouton_supp =
          Bouton.effacer ~style:Style.[marginHorizontal 20]
            ~onMouseUp:(fun _ -> dispatch Vider) ()
        in let sous_bout =
          View.createElement ~style:Style.[flexDirection `Row]
            ~children:[bouton_calc; bouton_supp] ()
        in let bouton_his =
          Bouton.menu_historique
            ~style:Style.[position `Absolute; left 10; bottom 10]
            ~onMouseUp:(fun _ -> changerVue `VueHistorique) ()
        in let bouton_acc =
          Bouton.menu_accueil
            ~style:Style.[position `Absolute; bottom 10; right 10]
            ~onMouseUp:(fun _ -> changerVue `VueAccueil) ()
        in let boutton_mat =
          Bouton.menu_matrices
            ~onMouseUp:(fun _ -> changerVue `VueMatrice)
            ~style:Style.[justifyContent `Center;
              position `Absolute; bottom 50; right 10]
            ()
        in let boutton_equ =
          Bouton.menu_equations
          ~onMouseUp:(fun _ -> changerVue `VueEquation)
          ~style:Style.[justifyContent `Center; position `Absolute;
            bottom 90; right 10] ()
        in let dimensions = Monitor.getPrimaryMonitor () |> Monitor.getSize in
        hooks,
          View.createElement ~style:containerStyle ~children:[
            Input.createElement
              ~style:Style.[color (Color.rgb 25. 5. 5.); bottom 0; top 0;
                left 0; right 0; width (
                  let taille = max 200 (String.length valeur * 10 + 20) in
                  if taille < dimensions.width / 2 - 20 then
                    taille
                  else
                    dimensions.width / 2 - 20
                )]
              ~value:valeur ~placeholder:"Entrer votre calcul"
              ~onChange:(fun {value; _} -> dispatch(MiseAJour value))
              ~children:[] ();
            Text.createElement ~text:res(* retour du moteur *) ~style:textStyle
              ~children:[] (); sous_bout; bouton_acc; bouton_his;
              boutton_equ; boutton_mat] ())
end

module Historique = struct
  let component = React.component "Historique"

  let createElement historique ~changerVue ~onUpdate:_ =
    let containerStyle =
      Style.[
        position `Absolute;
        justifyContent `Center;
        alignItems `Center;
        bottom 0;
        top 0;
        left 0;
        right 0
        ]
    in let textStyle =
      Style.[
        fontSize 25;
        left 0;
        right 0;
        bottom 0;
        top 0;
        fontFamily "Roboto-Regular.ttf"
      ]
    in let histol hist =
      let txt_gen = Text.createElement ~style:textStyle ~children:[] in
      List.map (fun text -> txt_gen ~text ()) hist
    in fun ~children:_ () ->
      component (fun hooks ->
        let bouton_retour =
          Bouton.menu_retour ~onMouseUp:(fun _ -> changerVue `VueCalcul)
            ~style:Style.[position `Absolute; bottom 10; right 10] ()
        in hooks,
          View.createElement ~style:containerStyle
            ~children:(bouton_retour :: (histol historique)) ())
end

module Matrice = struct
  let component = React.component "Matrice"

  type 'a action =
      MiseAJour of int * int * int * string(* * ('a -> unit) *)
    | MiseAJourEntete of int * string(* * ('a -> unit) *)
    | ChangerMode of matrice_mode
    | Calculer of matrice_mode

  let reducer action etat =
    match action with
      MiseAJour (id, i, j, v(*, f*)) ->
        let mat =
          if id = 1 then
            etat.matrice1
          else
            etat.matrice2
        in let () =
          try
            let _ = float_of_string v in
            mat.(i).(j) <- v
          with Failure _ -> ()
        in (*let () = f (`Matrice etat) in*)
        if id = 1 then
          {etat with matrice1 = mat}
        else
          {etat with matrice2 = mat}
      | MiseAJourEntete (j, v(*, f*)) ->
        let () = etat.matrice1.(0).(j) <- v in
        {etat with matrice1 = etat.matrice1}
      | ChangerMode m ->
        if m <> `Solveur then
          {
            etat with mode = m;
            taille2 = (let (h, _) = etat.taille1 in (h, 2));
            taille_res = (let (h, _) = etat.taille_res in (h, 2));
          }
        else
          {
            etat with mode = m;
            taille2 = (let (h, _) = etat.taille1 in (h, 1));
            taille_res = (let (h, _) = etat.taille_res in (h, 1));
          }
      | Calculer `Solveur ->
        etat
      | Calculer _ ->
        let array_map2 f = Array.map (fun e -> Array.map f e) in
        let m1 = array_map2 float_of_string etat.matrice1 in
        let m2 = array_map2 float_of_string etat.matrice2 in
        let f =
          (
            match etat.mode with
              `Addition -> Modules.Matrix.Test_float_matrix.additioner
            | `Soustraction -> Modules.Matrix.Test_float_matrix.soustraire
            | `Multiplication -> Modules.Matrix.Test_float_matrix.multiplier
            | _ -> Modules.Matrix.Test_float_matrix.additioner
          )
        in let matrice_res = f m1 m2 in
        let matrice_res = array_map2 string_of_float matrice_res in
        {
          etat with matrice_res;
          taille_res = (Array.length matrice_res, Array.length matrice_res.(0));
        }


  let createElement ~initialState ~changerVue ~onUpdate =
    fun ~children:_ () ->
      component (fun hooks ->
        let (etat (* nouvel etat *), dispatch, hooks) =
          React.Hooks.reducer ~initialState reducer hooks
        in let () = onUpdate (`Matrice etat) in
        let dessiner_matrice (h, w) f =
          let input = ref [] in
          let row_gen = View.createElement ~style:Style.[flexDirection(`Row)] in
          let () =
            for i = pred h downto 0 do
              let row = ref [] in
              let () =
                for j = pred w downto 0 do
                  row := (f i j) :: !row
                done
              in input := (row_gen ~children:!row ()) :: !input
            done
          in (View.createElement ~children:!input) ()
        in let input_gen =
          Input.createElement
            ~style:Style.[color (Color.rgb 255. 255. 255.); width 100;
              margin2 ~horizontal:40 ~vertical:10]
        in let m1 =
          dessiner_matrice etat.taille1 (fun i j -> input_gen
            ~value:etat.matrice1.(i).(j)
            ~placeholder:etat.matrice1.(0).(j)
            ~onChange:(fun {value; _} ->
              if i = 0 && etat.mode = `Solveur then
                dispatch(MiseAJourEntete (j, value(*, onUpdate *)))
              else
                dispatch(MiseAJour (1, i, j, value(*, onUpdate *)))
            )
            ~children:[] ())
        in let op: Dropdown.items =
          [
            {value = "+"; label = "+                             ";
            (* all this space are for a bug *)};
            {value = "-"; label = "-                             ";};
            {value = "*"; label = "*                             ";};
            (*{value = "resoudre"; label = "resoudre               ";};*)
          ]
        in let dropdown =
          Dropdown.createElement ~items:op
            ~onItemSelected:(fun {value = a; _} -> dispatch ((function
                "+" -> ChangerMode `Addition
              | "-" -> ChangerMode `Soustraction
              | "*" -> ChangerMode `Multiplication
              | "resoudre" -> ChangerMode `Solveur
              | _ -> failwith "impossible") a))
            ~children:[] ()
        in let m2 =
          dessiner_matrice etat.taille2 (fun i j -> input_gen
            ~value:etat.matrice2.(i).(j)
            ~placeholder:etat.matrice2.(0).(j)
            ~onChange:(fun {value; _} ->
              if i = 0 && etat.mode = `Solveur then
                dispatch(MiseAJourEntete (j, value(*, onUpdate *)))
              else
                dispatch(MiseAJour (2, i, j, value(*, onUpdate *)))
            )
            ~children:[] ())
        in let m_res =
          dessiner_matrice etat.taille_res (fun i j -> input_gen
            ~value:etat.matrice_res.(i).(j)
            ~children:[] ())
        in let bouton_calc =
          Bouton.calculer
            ~onMouseUp:(fun _ -> dispatch (Calculer etat.mode))
            ~style:Style.[position `Absolute; bottom 10; right 10] ()
        in let bouton_retour =
          Bouton.menu_accueil
            ~onMouseUp:(fun _ -> changerVue `VueAccueil)
            ~style:Style.[position `Absolute; bottom 10; left 10] ()
        in let boutton_equ =
          Bouton.menu_equations ~onMouseUp:(fun _ -> changerVue `VueEquation)
            ~style:Style.[justifyContent `Center; position `Absolute;
              bottom 90; left 10] ()
        in let boutton_cal =
          Bouton.menu_calcul ~onMouseUp:(fun _ -> changerVue `VueCalcul)
          ~style:Style.[justifyContent `Center; position `Absolute;
            bottom 50; left 10] ()
        in let children1 =
          [bouton_retour; bouton_calc; boutton_cal; boutton_equ]
        in let chmttaille id c d (a, b) =
          match id with
            1 when d ->
              if c then
                etat.taille1 <- (a + 1, b)
              else if a > 1 then
                etat.taille1 <- (a - 1, b)
          | 1 ->
            if c then
              etat.taille1 <- (a, b + 1)
            else if b > 1 then
              etat.taille1 <- (a, b - 1)
          | 2 when d ->
            if c then
              etat.taille2 <- (a + 1, b)
            else if a > 1 then
              etat.taille2 <- (a - 1, b)
          | 2 ->
            if c then
              etat.taille2 <- (a, b + 1)
            else if b > 1 then
              etat.taille2 <- (a, b - 1)
          | 3 when d ->
            if c then
              etat.taille_res <- (a + 1, b)
            else if a > 1 then
              etat.taille_res <- (a - 1, b)
          | _ ->
            if c then
              etat.taille_res <- (a, b + 1)
            else if b > 1 then
              etat.taille_res <- (a, b - 1)
        in let chmtmat id (a, b) =
          match id with
              1 -> etat.matrice1 <- Array.make_matrix a b "0"
            | 2 -> etat.matrice2 <- Array.make_matrix a b  "0"
            | _ -> etat.matrice_res <- Array.make_matrix a b "0"
        (*in let squareu id signe =
          match id with
            1 -> chmttaille 1 signe true etat.taille1;
              chmttaille 1 signe false etat.taille1; chmtmat 1 etat.taille1
          | 2 -> chmttaille 2 signe true etat.taille2;
            chmttaille 2 signe false etat.taille2; chmtmat 2 etat.taille2
          | _ -> chmttaille 3 signe true etat.taille_res;
            chmttaille 3 signe false etat.taille_res;
              chmtmat 3 etat.taille_res*)
        in let mat1cou =
          Bouton.plus
            ~onMouseUp:(fun _ -> (*if etat.mode = `Multiplication then
              begin*)
              chmttaille 1 true true etat.taille1; chmtmat 1 etat.taille1;
              (*chmttaille 3 true true etat.taille_res;
                chmtmat 3 etat.taille_res;*)
              dispatch(MiseAJour (1, 0, 0, etat.matrice1.(0).(0))) (*
            end
            else
            begin
              squareu 1 true; squareu 2 true; squareu 3 true;
              dispatch(MiseAJour (1, 0, 0, etat.matrice1.(0).(0)));
            end*) ) ~style:[] ()
        in let mat1cod =
          Bouton.moins ~style:Style.[marginHorizontal 20]
            ~onMouseUp:(fun _ -> (*if etat.mode = `Multiplication then
              begin*)
              chmttaille 1 false true etat.taille1; chmtmat 1 etat.taille1;
              (*chmttaille 3 false true etat.taille_res;
              chmtmat 3 etat.taille_res;*)
              dispatch(MiseAJour (1, 0, 0, etat.matrice1.(0).(0))) (*
            end
            else
            begin
              squareu 1 false; squareu 2 false; squareu 3 false;
              dispatch(MiseAJour (1, 0, 0, etat.matrice1.(0).(0)))
            end*)) ()
        in let boutton_mat1c =
          View.createElement
            ~style:Style.[flexDirection `Row; flexGrow 2;
              marginHorizontal 50]
            ~children:[mat1cou; mat1cod] ()
        in let mat2cou =
          Bouton.plus ~style:Style.[marginHorizontal 20]
            ~onMouseUp:(fun _ ->
              chmttaille 2 true true etat.taille2; chmtmat 2 etat.taille2;
              dispatch(MiseAJour (2, 0, 0, etat.matrice2.(0).(0)))) ()
        in let mat2cod =
          Bouton.moins
            ~onMouseUp:(fun _ ->
              chmttaille 2 false true etat.taille2; chmtmat 2 etat.taille2;
              dispatch(MiseAJour (2, 0, 0, etat.matrice2.(0).(0)))) ()
        in let boutton_mat2c =
          View.createElement
            ~style:Style.[flexDirection `RowReverse; marginHorizontal 50]
            ~children:(
              (*if etat.mode = `Multiplication then*)
                [mat2cou; mat2cod]
              (*else
                [mat1cou; mat1cod]*)) ()
        in let boutton_matc =
          View.createElement ~style:Style.[flexDirection`Row]
            ~children:[boutton_mat1c; boutton_mat2c] ()
        in let mat1liu =
          Bouton.plus
          ~onMouseUp:(fun _ -> (*if etat.mode = `Multiplication then
            begin*)
            chmttaille 1 true false etat.taille1; chmtmat 1 etat.taille1;
          dispatch(MiseAJour (1, 0, 0, etat.matrice1.(0).(0)))
          (*end
          else
          begin
            squareu 1 true; squareu 2 true; squareu 3 true;
            dispatch(MiseAJour (1, 0, 0, etat.matrice1.(0).(0)))
          end*)) ()
        in let mat1lid = Bouton.moins
            ~onMouseUp:(fun _ ->(*if etat.mode = `Multiplication then
              begin*)
              chmttaille 1 false false etat.taille1; chmtmat 1 etat.taille1;
            dispatch(MiseAJour (1, 0, 0, etat.matrice1.(0).(0)))
            (*end
            else
            begin
              squareu 1 false; squareu 2 false; squareu 3 false;
              dispatch(MiseAJour (1, 0, 0, etat.matrice1.(0).(0)))
            end*)) ()
        in let boutton_mat1l =
          View.createElement ~children:[mat1liu; mat1lid] ()
        in let mat2liu =
          Bouton.plus
            ~onMouseUp:(fun _ ->
                chmttaille 2 true false etat.taille2; chmtmat 2 etat.taille2;
                (*chmttaille 3 true false etat.taille_res;
                chmtmat 3 etat.taille_res;*)
                dispatch(MiseAJour (2, 0, 0, etat.matrice2.(0).(0)))) ()
        in let mat2lid =
          Bouton.moins
            ~onMouseUp:(fun _ ->
              chmttaille 2 false false etat.taille2; chmtmat 2 etat.taille2;
              (*chmttaille 3 false false etat.taille_res;
              chmtmat 3 etat.taille_res;*)
              dispatch(MiseAJour (2, 0, 0, etat.matrice2.(0).(0)))) ()
        in let boutton_mat2l =
          View.createElement
            ~children:(
              (*if etat.mode = `Multiplication then*)
                [mat2liu; mat2lid]
              (*else
                [mat1liu; mat1lid]*)) ()
        in let children =
          View.createElement ~style:Style.[flexDirection `Row]
            ~children:[boutton_mat1l; m1; dropdown; m2; boutton_mat2l] ()
        in hooks,
          View.createElement
            ~style:Style.[position `Absolute; alignItems `Center; bottom 0;
              top 0; left 0; right 0]
            ~children:(boutton_matc :: children :: m_res :: children1) ())
end

module Equation = struct

  let component = React.component "Equation"

  type 'a action =
      MiseAJour of int * int * string
    | MiseAJourVariable of int * int * string
    | Calculer of int

  let reducer action etat =
    match action with
      MiseAJour (i, j, v) ->
        let mat = etat.mat1
        in let _ = mat.(i).(j) <- v in
        {etat with mat1 = mat}
      | MiseAJourVariable (i, j, v) ->
        let mat = etat.mat2
        in let _ = mat.(i).(j) <- v in
        {etat with mat2 = mat}
      | Calculer _ ->
        let array_map2 f = Array.map (fun e -> Array.map f e) in
        let m1 = array_map2 float_of_string etat.mat1 in
        let m2 = array_map2 float_of_string etat.mat2 in
        let f = (fun _ -> "0" ) in
        let res = "" (* f m1 m2 *) in
        {etat with res}


  let createElement ~initialState ~changerVue ~onUpdate =
    fun ~children:_ () ->
      component (fun hooks ->
        let (etat (* nouvel etat *), dispatch, hooks) =
          React.Hooks.reducer ~initialState reducer hooks
        in let () = onUpdate (`Equation etat) in
        let bouton_acc =
          Bouton.menu_accueil
            ~onMouseUp:(fun _ -> changerVue `VueAccueil)
            ~style:Style.[justifyContent `Center; position `Absolute;
              bottom 10; left 10] ()
        in let bouton_calc =
          Bouton.calculer ~onMouseUp:(fun _ -> dispatch (Calculer 1))
            ~style:Style.[position `Absolute; bottom 10; right 10] ()
        in let egal =
          Text.createElement ~text:"=" ~style:Style.[fontSize 25;
            fontFamily "Roboto-Regular.ttf"; marginVertical 15] ~children:[] ()
        in let boutton_mat =
          Bouton.menu_matrices
            ~onMouseUp:(fun _ -> changerVue `VueMatrice)
            ~style:Style.[justifyContent `Center; position `Absolute;
              bottom 90; left 10] ()
        in let boutton_cal =
          Bouton.menu_calcul ~onMouseUp:(fun _ -> changerVue `VueCalcul)
            ~style:Style.[justifyContent `Center; position `Absolute;
              bottom 50; left 10] ()
        in let rec inc_to_list = function
            [] -> []
          | e :: l -> (Text.createElement ~text:e ~style:Style.[
                        fontSize 25; fontFamily "Roboto-Regular.ttf";
                        marginHorizontal 95]
                        ~children:[] ()) :: inc_to_list l
        in let dessiner_matrice (h, w) f =
          let input = ref [] in
          let row_gen = View.createElement ~style:Style.[flexDirection `Row] in
          let () =
            for i = pred h downto 0 do
              let row = ref [] in
              let () =
                for j = pred w downto 0 do
                  row := (f i j) :: !row
                done
              in input := (row_gen ~children:!row ()) :: !input
            done
          in (View.createElement ~children:!input) ()
        in let m1 =
          dessiner_matrice (etat.lines, etat.nbr_inc) (fun i j ->
            Input.createElement
              ~style:Style.[color (Color.rgb 255. 255. 255.); width 100;
                margin2 ~horizontal:40 ~vertical:10]
              ~value:etat.mat1.(i).(j)
              ~placeholder:etat.mat1.(i).(j)
              ~onChange:(fun {value; _} -> dispatch(MiseAJour (i, j, value)))
              ~children:[] ())
        in let m2 =
          dessiner_matrice (etat.lines, 1) (fun i j -> Input.createElement
            ~style:Style.[color (Color.rgb 255. 255. 255.); width 100;
              margin2 ~horizontal:40 ~vertical:10]
            ~value:etat.mat2.(i).(j)
            ~placeholder:etat.mat2.(i).(j)
            ~onChange:(fun {value; _} ->
              dispatch(MiseAJourVariable (i, j, value)))
            ~children:[] ())
        in let add_inc () =
          let suf = List.nth etat.inconnu (etat.nbr_inc - 1)
          in let inc = 1 + int_of_char suf.[0] in
          let inc =
            let inc = if inc > 122 then inc - 26 else inc in
            String.make 1 (char_of_int inc)
          in let inc = (
            if (List.exists (fun a -> String.equal a inc) etat.inconnu) then
              String.make ((String.length (suf)) + 1) inc.[0]
            else
              inc)
          in let _ = etat.inconnu <- etat.inconnu @ [inc] in
          etat.nbr_inc <- etat.nbr_inc + 1
        in let minus_inc () =
          if etat.nbr_inc > 1 then
            let (_ :: l) = List.rev etat.inconnu in
            let () = etat.inconnu <- List.rev l in
            let () = etat.nbr_inc <- etat.nbr_inc - 1 in
            etat.mat1 <- Array.make_matrix 1 etat.nbr_inc "0"
        in let boutton_addinc =
          Bouton.plus ~style:Style.[marginHorizontal 10] ~onMouseUp:(fun _ ->
          add_inc ();
          etat.mat1 <- Array.make_matrix etat.lines etat.nbr_inc "0";
          dispatch(MiseAJour (0, 0, etat.mat1.(0).(0)))) ()
        in let boutton_mininc =
          Bouton.moins ~style:Style.[marginHorizontal 10]
            ~onMouseUp:(fun _ -> minus_inc ();
            dispatch(MiseAJour (0, 0, etat.mat1.(0).(0)))) ()
      (*in let boutton_addline =
          Text.createElement ~text:"+" ~onMouseUp:(fun _ ->
            etat.lines <- etat.lines + 1;
            etat.mat1 <- Array.make_matrix etat.lines etat.nbr_inc "0";
            etat.mat2 <- Array.make_matrix etat.lines 1 "0";
            dispatch(MiseAJour (1, 0, 0, "0")))
          ~style:Style.[fontSize 25; fontFamily "Roboto-Regular.ttf";
            marginHorizontal 10]
          ~children:[] ()
        in let boutton_minline =
          Bouton.moins ~onMouseUp:(fun _ ->
            if etat.lines > 1 then
              begin
              etat.lines <- etat.lines - 1;
              etat.mat1 <- Array.make_matrix etat.lines etat.nbr_inc "0";
              etat.mat2 <- Array.make_matrix etat.lines 1 "0";
              dispatch(MiseAJour (1, 0, 0, "0"))
              end )
          ~style:Style.[fontSize 25; fontFamily "Roboto-Regular.ttf";
            marginHorizontal 10]
          ~children:[] ()*)
        in let children =
          View.createElement ~children:[boutton_addinc; boutton_mininc] ()
        in let list_inc =
          View.createElement
            ~style:Style.[flexDirection `Row; alignSelf `FlexStart]
            ~children:(children :: (inc_to_list etat.inconnu)) ()
        in let children =
          View.createElement
            ~style:Style.[flexDirection `Row; alignSelf `FlexStart]
            ~children:[m1; egal; m2] ()
        (*in let line = View.createElement
          ~style:Style.[flexDirection `Row; alignSelf `FlexStart]
          ~children:[boutton_addline; boutton_minline] ()*)
        in let resultat =
          Text.createElement ~text:etat.res
            ~style:Style.[fontSize 25; fontFamily "Roboto-Regular.ttf"]
            ~children:[] ()
        in hooks,
          View.createElement
            ~style:Style.[position `Absolute; alignItems `Center; bottom 0;
              top 0; left 0; right 0]
            ~children:[list_inc; children; resultat;
              (*line;*) bouton_acc; boutton_cal; boutton_mat; bouton_calc] ())
end

module Accueil = struct
  let component = React.component "Accueil"

  type action =
    ChangerLangue of I18n.lang

  let reducer action etat =
    match action with
      ChangerLangue lang ->
        let () = I18n.definir_lang lang in
        {lang}

  let createElement ~initialState ~changerVue ~onUpdate =
  fun ~children:_ () ->
    component (fun hooks ->
      let (etat (* nouvel etat *), dispatch, hooks) =
        React.Hooks.reducer ~initialState reducer hooks
      in let () = onUpdate (`Accueil etat) in
      let containerStyle =
        Style.[
          position `Absolute;
          justifyContent `Center;
          alignItems `Center;
          bottom 0;
          top 0;
          left 0;
          right 0
          ]
      in let translate1, hooks =
        Hooks.animation (Animated.floatValue (-250.))
        {
          toValue = 20.;
          duration = Seconds 2.5;
          delay = Seconds 0.;
          repeat = false;
          easing = Easing.linear;
          direction = `Normal;
        } hooks
      in let translate2, hooks =
        Hooks.animation (Animated.floatValue 150.)
        {
          toValue = -90.;
          duration = Seconds 5.;
          delay = Seconds 2.5;
          repeat = false;
          easing = Easing.linear;
          direction = `Normal
        } hooks
      in let scale, hooks =
        Hooks.animation (Animated.floatValue 0.)
        {
          toValue = 1.;
          duration = Seconds 5.;
          delay = Seconds 2.5;
          repeat = false;
          easing = Easing.linear;
          direction = `Normal
        } hooks
      in let imageStyle1 =
        Style.[
          bottom 0;
          top 0;
          left 0;
          right 0;
          width 200;
          height 200;
          transform [Transform.TranslateX translate1]
        ]
      in let imageStyle2 =
        Style.[
          top 0;
          bottom 0;
          right 0;
          left 0;
          width 75;
          height 75;
          transform [
          Transform.TranslateY translate2;
          Transform.ScaleX scale;
          Transform.ScaleY scale;
          ];
        ]
      in let bouton_cal =
        Bouton.menu_calcul ~onMouseUp:(fun _ -> changerVue `VueCalcul)
          ~style:Style.[justifyContent `Center; color (Color.rgb 255. 120. 10.);
            position `Absolute; bottom 10; left 10] ()
      in let bouton_mat =
        Bouton.menu_matrices ~onMouseUp:(fun _ -> changerVue `VueMatrice)
          ~style:Style.[justifyContent `Center; color (Color.rgb 255. 120. 10.);
            position `Absolute; right 10; bottom 10] ()
      in let bouton_equa =
        Bouton.menu_equations ~onMouseUp:(fun _ -> changerVue `VueEquation)
          ~style:Style.[
            justifyContent `Center; color (Color.rgb 255. 120. 10.);
            position `Absolute; bottom 50; right 10] ()
      in let bouton_langue =
        let src = "drapeau_" ^ (I18n.obtenir_lang ()) ^ ".png"
        and style =
          Style.[position `Absolute; top 10; right 10; height 25; width 38]
        and onMouseUp _ =
          let nouvelle_langue =
            match I18n.obtenir_lang () with
              "fr" -> I18n.Anglais
            | "eng" -> I18n.Francais
            | _ -> failwith "langue non reconnu"
          in dispatch (ChangerLangue nouvelle_langue)
        in Image.createElement ~src ~style ~onMouseUp ~children:[] ()
      in hooks,
        View.createElement ~style:containerStyle
          ~children:[bouton_langue;
            Image.createElement
              ~onMouseUp:(fun _ -> changerVue `VueBonus) ~src:"pi.png"
              ~style:imageStyle2 ~children:[] ();
            Image.createElement ~src:"camel.png" ~style:imageStyle1
              ~children:[] ();
            bouton_cal; bouton_equa; bouton_mat;
          ] ())
end

module Application = struct
  let component = React.component "Application"

  type action =
      ChangerVue of vue

  let sauvegarde =
    ref {
      equation =
      {
        inconnu = ["x"];
        nbr_inc = 1;
        lines = 1;
        mat1 = Array.make_matrix 1 1 "0";
        mat2 = Array.make_matrix 1 1 "0";
        res = "";
      };
      calcul = {
        valeur = "";
        res = " "(* trick to be displayed *);
        liste_historique = !Commune.liste_historique;
          (* TODO: avoid this double list *)
        context = Commune.init_context
      };
      historique = {
        liste = [];
      };
      matrice = {
        mode = `Addition;
        matrice1 = Array.make_matrix 2 2 "0";
        matrice2 = Array.make_matrix 2 2 "0";
        matrice_res = Array.make_matrix 2 2 "0";
        taille1 = (2, 2);
        taille2 = (2, 2);
        taille_res = (2, 2);
        message = "";
      };
      accueil = {
        lang = I18n.Francais;
      };
    }

  let reducer action _ (* etat *) =
    match action with
      ChangerVue v -> {(* etat with *) vue_courante = v}

  let miseAJour = function
      `Calcul e ->
        let () = sauvegarde := {!sauvegarde with calcul = e} in
          !sauvegarde.historique.liste <- e.liste_historique
    | `Matrice e -> sauvegarde := {!sauvegarde with matrice = e}
    | `Accueil e -> sauvegarde := {!sauvegarde with accueil = e}
    | `Equation e -> sauvegarde := {!sauvegarde with equation = e}

  let createElement =
    fun ~children:_ () ->
      component(fun hooks ->
          let {vue_courante}, dispatch, hooks =
            React.Hooks.reducer ~initialState:{vue_courante = `VueAccueil}
              reducer hooks
          in let choisir_vue = function
              `VueCalcul -> Calcul.createElement
                ~initialState:(!sauvegarde.calcul)
            | `VueHistorique -> Historique.createElement
              !sauvegarde.calcul.liste_historique
            | `VueMatrice -> Matrice.createElement
              ~initialState:(!sauvegarde.matrice)
            | `VueAccueil -> Accueil.createElement
              ~initialState:(!sauvegarde.accueil)
            | `VueEquation -> Equation.createElement
              ~initialState:(!sauvegarde.equation)
            | `VueBonus -> VueBonus.createElement
          in hooks,
            (choisir_vue vue_courante)
            ~changerVue:(fun v -> dispatch (ChangerVue v))
            ~onUpdate:miseAJour
            ~children:[] ())
end

(*
let init app =
let win = App.createWindow app "OCalc" in
Input.createElement ~style:Style.[top (100); left (300); position `Absolute]
  ~children:[] () |>
UI.start win |> ignore

let init app =
  let win = App.createWindow app "OCalc" in
  Input.createElement ~style:Style.[] ~children:[] () |>
  UI.start win |> ignore

let init app =
  let win = App.createWindow app "OCalc" in
  let op: Dropdown.items = [
    {value = "+"; label = "+";};
    {value = "-"; label = "-";};
    {value = "*"; label = "*";};
  ] in Dropdown.createElement ~items:op ~onItemSelected:(fun _ -> ())
    ~children:[] () |>
  UI.start win |> ignore
*)

let init app =
  let maximized = Environment.webGL in
  let dimensions = Monitor.getPrimaryMonitor () |> Monitor.getSize in
  let windowWidth = dimensions.width / 2 in
  let windowHeight = dimensions.height / 2 in
  let options_fen =
    WindowCreateOptions.create ~width:windowWidth ~height:windowHeight
      ~maximized ~icon:(Some "logo.png") ~backgroundColor:bgColor ()
  in let fen = App.createWindow ~createOptions:options_fen app "OCalc" in
  let () =
    if not Environment.webGL then
      let xPosition = (dimensions.width - windowWidth) / 2 in
      let yPosition = (dimensions.height - windowHeight) / 2 in
      Window.setPos fen xPosition yPosition
  in let afficher () = Application.createElement ~children:[] () in
  UI.start fen (afficher ()) (afficher ())

let _ = App.start init
