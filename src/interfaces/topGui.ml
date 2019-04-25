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
type matrice_state =
{
  mode: [`Addition | `Soustraction | `Multiplication | `division];
  matrice1: int array array;
  matrice2: int array array;
  taille1: int * int;
  taille2: int * int;
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
      let res, cxt =
        try
          Noyau.Moteur.evaluate_with_history etat.valeur etat.context
        with Failure msg -> msg, etat.context (* old context *)
      in {liste_historique = res :: etat.liste_historique; context = cxt; valeur = ""; res}

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
            Button.createElement ~title:"Calculer" ~width:150 ~height:50 ~fontSize:25 ~onClick:(fun _ -> dispatch Calculer) ~children:[] ();
            Button.createElement ~title:"Éffacer" ~width:150 ~height:50 ~fontSize:25 ~onClick:(fun _ -> dispatch Vider) ~children:[] ();
            Button.createElement ~title:"Accéder à l'historique" ~width:175
              ~fontSize:25  ~onClick:(fun _  -> changerVue `VueHistorique)
              ~children:[] ();
            Button.createElement ~title:"Revenir à l'accueil" ~width:175
              ~fontSize:25  ~onClick:(fun _  -> changerVue `VueAccueil)
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
      fontFamily "Roboto-Regular.ttf"
      ]
    in
    let rec histol hist res =
      match hist with
       [] -> []
      | e :: l -> histol l ((Text.createElement ~text:e ~style:textStyle ~children:[]) :: res)
    in
    fun ~children:_ () ->
      component
        (fun hooks ->
          let bouton_retour = Button.createElement ~title:"Revenir au mode Équation"
               ~width:175
               ~fontSize:25
               ~onClick:(fun _  ->
                          changerVue `VueEquation)
               ~children:[] () in
          hooks, View.createElement ~style:containerStyle ~children:(bouton_retour :: (histol historique [])) ())
end

module Matrice = struct
  let component = React.component "Matrice"

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
        in let () = onUpdate (`Matrice etat) in
        (hooks, View.createElement ~children:[
          Button.createElement ~title:"Revenir à l'accueil" ~width:175
            ~fontSize:25  ~onClick:(fun _  -> changerVue `VueAccueil)
            ~children:[] ();
          ] ()))
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
          direction = `Normal
        } hooks
        in let translate2, hooks =
          Hooks.animation (Animated.floatValue 150.)
          {
            toValue = -100.;
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
     (hooks, View.createElement ~style:containerStyle ~children:[
        Image.createElement ~src:"pi.png" ~style:imageStyle2 ~children:[] ();
        Image.createElement ~src:"camel.png" ~style:imageStyle1 ~children:[] ();
        Button.createElement ~title:"Accéder à équation" ~width:175
           ~fontSize:25 ~onClick:(fun _ -> changerVue `VueEquation)
           ~children:[] ();
         Button.createElement ~title:"Accéder à matrice" ~width:175
          ~fontSize:25 ~onClick:(fun _ -> changerVue `VueMatrice)
          ~children:[] ();
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
      matrice1 = Array.create_matrix 3 3 0;
      matrice2 = Array.create_matrix 3 3 0;
      taille1 = (3, 3);
      taille2 = (3, 3);
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
  let options_fen = WindowCreateOptions.create ~icon:(Some "logo.png") () in
  let fen = App.createWindow ~createOptions:options_fen app "OCalc" in
  let afficher () = Application.createElement ~children:[] () in
  UI.start fen (afficher ()) (afficher ())

let _ = App.start init
