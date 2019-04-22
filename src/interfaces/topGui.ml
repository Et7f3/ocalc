open Revery
open Revery.UI
open Revery.UI.Components

type main_view_state =
{
  valeur: string;
}

module VuePrincipal = struct
  let component =
    React.component "VuePrincipal"

  type action =
    Vider
  | MiseAJour of string
  | Calculer

  let reducer action etat =
    match action with
      Vider -> {valeur = ""}
    | MiseAJour valeur -> {valeur} (* ici on met à jour notre état *)
    | Calculer -> (* on doit calculer ici et vider l'entre
    BoNuS: si c'est valide: tapez élie en attendant *) {valeur = ""}

  let createElement =
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
    fun ~children:_ () ->
      component
        (fun hooks  ->
          let ({valeur}, dispatch,hooks) =
            React.Hooks.reducer ~initialState:{valeur = ""} reducer hooks
          in
          (hooks, View.createElement ~style:containerStyle ~children:[
            Input.createElement ~value:valeur ~placeholder:"Entrer votre équation" ~onChange:(fun {value; _} -> dispatch(MiseAJour value)) ~children:[] ();
            Button.createElement ~title:"Calculer" ~onClick:(fun _ -> dispatch Calculer) ~children:[] ();
            Button.createElement ~title:"Éffacer" ~onClick:(fun _ -> dispatch Vider) ~children:[] ()
            ] ()))
end

let init app =
  let fen =
  App.createWindow app "OCalc"
  in
  let afficher () = VuePrincipal.createElement ~children:[] () in
  UI.start fen (afficher ()) (afficher ()) (* MAJ: je ne sai spas pk il faut le mettre 2 fois *)

let _ = App.start init
