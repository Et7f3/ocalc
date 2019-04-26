open Revery
open Revery.UI
open Revery.UI.Components

type vue =
  [
      `VueEquation
    | `VueHistorique
    | `VueMatrice
    | `VueAccueil
  ]

(*
type vue_par_defaut_state =
{
  nothing: unit;
}
*)
type equation_state =
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

type matrice_mode = [`Solveur | `Addition | `Soustraction | `Multiplication | `Inverse]

type matrice_state =
{
  mode: matrice_mode;
  matrice1: string array array;
  matrice2: string array array;
  matrice_res: string array array;
  taille1: int * int;
  taille2: int * int;
  taille_res: int * int;
  message: string;
}

type accueil_state =
{
  nothing: unit
}

type application_state =
{
  vue_courante: vue;
}

type application_sauvegarde =
{
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
      (fun hooks  ->
        let ({_} (* nouvel etat *) as etat, _ (* dispatch *), hooks) =
          React.Hooks.reducer ~initialState reducer hooks
        in let () = onUpdate (`VueParDefaut etat) in
        (hooks, View.createElement ~children:[] ()))
end
*)


module Equation = struct
  let component = React.component "Equation"

  type action =
      Vider
    | MiseAJour of string
    | Calculer

  let reducer action etat =
    match action with
      Vider -> {etat with valeur = ""}
    | MiseAJour valeur -> {etat with valeur} (* ici on met à jour notre état *)
    | Calculer ->
      let rec restr strl =
      match strl with
      [] -> ""
      |e :: l -> e ^ restr l
      in
      let res = etat.valeur in
      let res = restr (String.split_on_char ' ' res) in
      let res, cxt =
        try
          Noyau.Moteur.evaluate_with_history res etat.context
        with Failure msg -> msg, etat.context (* old context *)
      in let res = etat.valeur ^ " = " ^ res in
      {liste_historique = res :: etat.liste_historique; context = cxt; valeur = ""; res}

  let createElement ~initialState ~changerVue ~onUpdate =
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
  in
  let textStyle =
    Style.[
    fontSize 25;
    fontFamily "Roboto-Regular.ttf"
    ]
  in
  fun ~children:_ () ->
    component
      (fun hooks  ->
        let ({valeur; res; _} as etat, dispatch , hooks) =
          React.Hooks.reducer ~initialState reducer hooks
        in let () = onUpdate (`Equation etat) in
        (hooks,
          View.createElement ~style:containerStyle ~children:[
            Input.createElement ~value:valeur ~placeholder:"Entrer votre équation" ~onChange:(fun {value; _} -> dispatch(MiseAJour value)) ~children:[] ();
            Text.createElement ~text:res(*retour du moteur*) ~style:textStyle ~children:[] ();
            Text.createElement ~text:"Calculer" ~style:Style.[fontSize 25;
            fontFamily "Roboto-Regular.ttf";
            top 0;
            left 0;
            bottom 0;
            right 0;] ~onMouseUp:(fun _ -> dispatch Calculer) ~children:[] ();
            Text.createElement ~text:"Éffacer" ~style:Style.[fontSize 25;
            fontFamily "Roboto-Regular.ttf";
            top 0;
            left 0;
            bottom 0;
            right 0;] ~onMouseUp:(fun _ -> dispatch Vider) ~children:[] ();
            Text.createElement ~text:"Accéder à l'historique" ~style:Style.[fontSize 25;
            fontFamily "Roboto-Regular.ttf";
            top 0;
            left 0;
            bottom 0;
            right 0;]  ~onMouseUp:(fun _  -> changerVue `VueHistorique)
              ~children:[] ();
            Text.createElement ~text:"Revenir à l'accueil" ~style:Style.[fontSize 25;
            fontFamily "Roboto-Regular.ttf";
            top 0;
            left 0;
            bottom 0;
            right 0;]  ~onMouseUp:(fun _  -> changerVue `VueAccueil)
              ~children:[] ();
            ] ()))
end

module Historique = struct
  let component =
    React.component "Historique"
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
    in
      let textStyle =
      Style.[
      fontSize 25;
      left 0;
      right 0;
      bottom 0;
      top 0;
      fontFamily "Roboto-Regular.ttf"
      ]
    in
    let histol hist =
      List.map (fun a -> Text.createElement ~text:a ~style:textStyle ~children:[] ()) hist
    in
    fun ~children:_ () ->
      component
        (fun hooks ->
          let bouton_retour = Text.createElement ~text:"Revenir au mode Équation"
          ~style:Style.[width 175; height 75; fontSize 25; fontFamily "Roboto-Regular.ttf";
          position `Absolute; top 500; left 0; right 0;]
               ~onMouseUp:(fun _  ->
                          changerVue `VueEquation)
               ~children:[] () in
          hooks, View.createElement ~style:containerStyle ~children:(bouton_retour :: (histol historique)) ())
end

module Matrice = struct
  let component = React.component "Matrice"

  type 'a action =
      MiseAJour of int * int * int * string(* * ('a -> unit) *)
    | MiseAJourEntete of int * string(* * ('a -> unit) *)
    | ChangerMode of matrice_mode
    | Calculer

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
      | ChangerMode m -> {etat with mode = m}
      | Calculer ->
        let array_map2 f = Array.map (fun e -> Array.map f e) in
        let m1 = array_map2 float_of_string etat.matrice1 in
        let m2 = array_map2 float_of_string etat.matrice2 in
        let f = (match etat.mode with
            `Addition -> Modules.Matrix.Test_float_matrix.additioner
          | `Soustraction -> Modules.Matrix.Test_float_matrix.soustraire
          | `Multiplication -> Modules.Matrix.Test_float_matrix.multiplier
          | _ -> Modules.Matrix.Test_float_matrix.additioner
        ) in
        let matrice_res = f m1 m2 in
        let matrice_res = array_map2 string_of_float matrice_res in
        {etat with matrice_res}


  let createElement ~initialState ~changerVue ~onUpdate =
  fun ~children:_ () ->
    component
      (fun hooks ->
        let (etat (* nouvel etat *), dispatch, hooks) =
          React.Hooks.reducer ~initialState reducer hooks
        in let () = onUpdate (`Matrice etat) in
        let bouton_calc =
          Text.createElement ~text:"Calculer le résultat"
           ~onMouseUp:(fun _  -> dispatch Calculer)
           ~style:Style.[
            width 175; height 75; fontSize 25; fontFamily "Roboto-Regular.ttf";
            position `Absolute; top 500; left 0;
          ]
           ~children:[] ()
        in let bouton_retour =
          Text.createElement ~text:"Revenir à l'accueil"
            ~onMouseUp:(fun _  -> changerVue `VueAccueil)
            ~style:Style.[
              width 175; height 75 ; fontSize 25; fontFamily "Roboto-Regular.ttf";
              position `Absolute; top 500; right 0;
            ]
            ~children:[] ()
        in let children = [bouton_retour; bouton_calc] in
        let dessiner_matrice (h, w) f =
          let input = ref [] in
          let () =
            for i = pred h downto 0 do
              let row = ref [] in
              let () =
                for j = pred w downto 0 do
                  row := (f i j) :: !row
                done
              in input :=  (View.createElement ~style:Style.[flexDirection(`Row)] ~children:!row ()) :: !input
            done
          in (View.createElement ~children:!input) ()
        in let m1 =
          dessiner_matrice etat.taille1 (fun i j -> Input.createElement
            ~style:Style.[width 100; margin2 ~horizontal:40 ~vertical:10]
            ~value:etat.matrice1.(i).(j)
            ~placeholder:etat.matrice1.(0).(j)
            ~onChange:(fun {value; _} ->
              if i = 0 && etat.mode = `Solveur then
                dispatch(MiseAJourEntete (j, value(*, onUpdate *)))
              else
                dispatch(MiseAJour (1, i, j, value(*, onUpdate *)))
            )
            ~children:[] ())
        in let op: Dropdown.items = [
          {value = "+"; label = "+                             ";(* all this space are for a bug *)};
          {value = "-"; label = "-                             ";};
          {value = "*"; label = "*                             ";};
        ] in
        let dropdown = Dropdown.createElement ~items:op
          ~onItemSelected:(fun {value = a; _} -> dispatch ((function
          | "+" -> ChangerMode `Addition
          | "-" -> ChangerMode `Soustraction
          | "*" -> ChangerMode `Multiplication
          | _ -> failwith ("impossible")) a))
          ~children:[] ()
        in let m2 =
          dessiner_matrice etat.taille2 (fun i j -> Input.createElement
            ~style:Style.[width 100; margin2 ~horizontal:40 ~vertical:10]
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
          dessiner_matrice etat.taille_res (fun i j -> Input.createElement
            ~style:Style.[width 100; margin2 ~horizontal:40 ~vertical:10]
            ~value:etat.matrice_res.(i).(j)
            ~children:[] ())
        in let children = (View.createElement ~style:Style.[flexDirection(`Row)] ~children:[m1; dropdown; m2] ()) :: m_res :: children
        in (hooks, View.createElement (* ~style:Style.[] *) ~children:children ()))
end

module Accueil = struct
  let component = React.component "Accueil"

  type action = Nope

  let reducer action etat =
    match action with
    Nope -> etat

  let createElement ~initialState ~changerVue ~onUpdate =
  fun ~children:_ () ->
    component
    (fun hooks  ->
      let (etat (* nouvel etat *), _ (* dispatch *), hooks) =
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
          easing = Animated.linear;
          direction = `Normal;
        } hooks
      in let translate2, hooks =
        Hooks.animation (Animated.floatValue 150.)
        {
          toValue = -50.;
          duration = Seconds 5.;
          delay = Seconds 2.5;
          repeat = false;
          easing = Animated.linear;
          direction = `Normal
        } hooks
      in let scale, hooks =
        Hooks.animation (Animated.floatValue 0.)
        {
          toValue = 1.;
          duration = Seconds 0.5;
          delay = Seconds 2.5;
          repeat = false;
          easing = Animated.linear;
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
            Transform.ScaleY scale
          ]
        ]
      in
      let bouton_equa =
        Text.createElement ~text:"Accéder à équation"
         ~onMouseUp:(fun _  -> changerVue `VueEquation)
         ~style:Style.[
          width 175; height 75; fontSize 25; fontFamily "Roboto-Regular.ttf"; justifyContent `Center; color (Color.rgb 255. 120. 10.);
          position `Absolute; top 500; left 0;
        ]
         ~children:[] ()
      in let bouton_mat =
        Text.createElement ~text:"Accéder à matrice"
          ~onMouseUp:(fun _  -> changerVue `VueMatrice)
          ~style:Style.[
            width 175; height 75 ; fontSize 25; justifyContent `Center; fontFamily "Roboto-Regular.ttf"; color (Color.rgb 255. 120. 10.);
            position `Absolute; right 0 ; top 500;
          ]
          ~children:[] ()
      in
     (hooks, View.createElement ~style:containerStyle ~children:[
        Image.createElement ~src:"pi.png" ~style:imageStyle2 ~children:[] ();
        Image.createElement ~src:"camel.png" ~style:imageStyle1 ~children:[] ();
        bouton_equa; bouton_mat;
      ] ()))
end

module Application = struct
  let component = React.component "Application"

  type action =
      ChangerVue of vue

  let sauvegarde = ref {
    equation = {
      valeur = "";
      res = " "(* trick to be displayed *);
      liste_historique = []; (* TODO: avoid this double list *)
      context = Noyau.Moteur.empty_context
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
      nothing = ();
    };
  }

  let reducer action etat =
    match action with
      ChangerVue v -> {(* etat with *) vue_courante = v}

  let miseAJour = function
    `Equation e ->
      let () = sauvegarde := {!sauvegarde with equation = e} in
      !sauvegarde.historique.liste <- e.liste_historique
    | `Matrice e -> sauvegarde := {!sauvegarde with matrice = e}
    | `Accueil e -> sauvegarde := {!sauvegarde with accueil = e}

  let createElement =
    fun ~children:_ () ->
      component
        (fun hooks  ->
          let ({vue_courante}, dispatch, hooks) =
            React.Hooks.reducer ~initialState:{vue_courante = `VueAccueil} reducer hooks
          in let choisir_vue = function
              `VueEquation -> Equation.createElement ~initialState:(!sauvegarde.equation)
            | `VueHistorique -> Historique.createElement !sauvegarde.equation.liste_historique
            | `VueMatrice -> Matrice.createElement ~initialState:(!sauvegarde.matrice)
            | `VueAccueil -> Accueil.createElement ~initialState:(!sauvegarde.accueil)
          in hooks, (choisir_vue vue_courante)
          ~changerVue:(fun v -> dispatch (ChangerVue v))
          ~onUpdate:miseAJour
          ~children:[] ())
end

let init app =
  let options_fen = WindowCreateOptions.create ~icon:(Some "logo.png") ~backgroundColor:(Color.hex("#212733")) () in
  let fen = App.createWindow ~createOptions:options_fen app "OCalc" in
  let afficher () = Application.createElement ~children:[] () in
  UI.start fen (afficher ()) (afficher ())

(*
let init app =
  let win = App.createWindow app "OCalc" in
  Input.createElement ~style:Style.[top (100); left (300); position `Absolute] ~children:[] () |>
  UI.start win |> ignore


let init app =
  let win = App.createWindow app "OCalc" in
  let op: Dropdown.items = [
    {value = "+"; label = "+";};
    {value = "-"; label = "-";};
    {value = "*"; label = "*";};
  ] in Dropdown.createElement ~items:op ~onItemSelected:(fun _ -> ()) ~children:[] () |>
  UI.start win |> ignore
*)

let _ = App.start init
