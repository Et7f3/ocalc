open Revery
open Revery.UI
open Revery.UI.Components

type vue =
  [
    `VueEquation
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
  context: Noyau.Moteur.context;
}

type application_state =
{
  vue_courante: vue;
}

type application_sauvegarde =
{
  equation: equation_state;
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
    | Calculer -> (* on doit calculer ici et vider l'entre
    BoNuS: si c'est valide: tapez élie en attendant *) {etat with valeur = ""}

  let createElement ~initialState ~changerVue:_ ~onUpdate =
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
        let ({valeur; _} as etat, dispatch , hooks) =
          React.Hooks.reducer ~initialState reducer hooks
        in let () = onUpdate (`Equation etat) in
        (hooks,
          View.createElement ~style:containerStyle ~children:[
            Input.createElement ~value:valeur ~placeholder:"Entrer votre équation" ~onChange:(fun {value; _} -> dispatch(MiseAJour value)) ~children:[] ();
            Button.createElement ~title:"Calculer" ~width:150 ~height:50 ~fontSize:25 ~onClick:(fun _ -> dispatch Calculer) ~children:[] ();
            Button.createElement ~title:"Éffacer" ~width:150 ~height:50 ~fontSize:25 ~onClick:(fun _ -> dispatch Vider) ~children:[] ();
            Text.createElement ~text:""(*retour du moteur*) ~style:textStyle ~children:[] ()
            ] ()))
end

module Application = struct
  let component = React.component "Application"

  type action =
      ChangerVue of vue

  let sauvegarde = ref {
    equation = {valeur = "ddd"; context = Noyau.Moteur.empty_context}
  }

  let reducer action etat =
    match action with
      ChangerVue v -> {etat with vue_courante = v}

  let miseAJour = function
    `Equation e -> sauvegarde := {!sauvegarde with equation = e}

  let createElement =
    fun ~children:_ () ->
      component
        (fun hooks  ->
          let ({vue_courante}, dispatch, hooks) =
            React.Hooks.reducer ~initialState:{vue_courante = `VueEquation} reducer hooks
          in let choisir_vue = function
            `VueEquation -> Equation.createElement ~initialState:(!sauvegarde.equation)
          in hooks, (choisir_vue vue_courante)  ~changerVue:(fun v -> dispatch (ChangerVue v)) ~onUpdate:miseAJour ~children:[] ())
end

let init app =
  let options_fen = WindowCreateOptions.create ~icon:(Some "logo.png") () in
  let fen = App.createWindow ~createOptions:options_fen app "OCalc" in
  let afficher () = Application.createElement ~children:[] () in
  UI.start fen (afficher ()) (afficher ())

let _ = App.start init
