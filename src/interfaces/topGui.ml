open Revery
open Revery.UI
open Revery.UI.Components

type vue =
  [
      `VueEquation
    | `VueHistorique
  ]

(*
type vue_par_defaut_state =
{
  nothing : unit;
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
  mutable liste : string list;
}

type application_state =
{
  vue_courante: vue;
}

type application_sauvegarde =
{
  equation: equation_state;
  historique: historique_state;
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
          React.Hooks.reducer ~initialState:{nothing = ()} reducer hooks
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
            Button.createElement ~title:"Accéder à l'historique"
                 ~width:175
                 ~fontSize:25
                 ~onClick:(fun _  ->
                            changerVue `VueHistorique)
                 ~children:[] ()
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
    }
  }

  let reducer action etat =
    match action with
      ChangerVue v -> {etat with vue_courante = v}

  let miseAJour = function
    `Equation e ->
      let () = sauvegarde := {!sauvegarde with equation = e} in
      !sauvegarde.historique.liste <- e.liste_historique

  let createElement =
    fun ~children:_ () ->
      component
        (fun hooks  ->
          let ({vue_courante}, dispatch, hooks) =
            React.Hooks.reducer ~initialState:{vue_courante = `VueEquation} reducer hooks
          in let choisir_vue = function
              `VueEquation -> Equation.createElement ~initialState:(!sauvegarde.equation)
            | `VueHistorique -> Historique.createElement !sauvegarde.equation.liste_historique
          in hooks, (choisir_vue vue_courante) ~changerVue:(fun v -> dispatch (ChangerVue v)) ~onUpdate:miseAJour ~children:[] ())
end

let init app =
  let options_fen = WindowCreateOptions.create ~icon:(Some "logo.png") () in
  let fen = App.createWindow ~createOptions:options_fen app "OCalc" in
  let afficher () = Application.createElement ~children:[] () in
  UI.start fen (afficher ()) (afficher ())

let _ = App.start init
