open Revery
open Revery.UI
open Revery.UI.Components

type main_view_state =
{
  valeur: string;
  context: Noyau.Moteur.context;
}
let _ = Commune.argc

module VuePrincipal = struct
  let component =
    React.component "VuePrincipal"

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
    let textStyle =
      Style.[
      fontSize 25;
      fontFamily "Roboto-Regular.ttf"
      ]
    in
    fun ~children:_ () ->
      component
        (fun hooks  ->
          let ({valeur; _}, dispatch,hooks) =
            React.Hooks.reducer ~initialState:{valeur = ""; context = Noyau.Moteur.empty_context} reducer hooks
          in
          (hooks, View.createElement ~style:containerStyle ~children:[
            Input.createElement ~value:valeur ~placeholder:"Entrer votre équation" ~onChange:(fun {value; _} -> dispatch(MiseAJour value)) ~children:[] ();
            Button.createElement ~title:"Calculer" ~width:150 ~height:50 ~fontSize:25 ~onClick:(fun _ -> dispatch Calculer) ~children:[] ();
            Button.createElement ~title:"Éffacer" ~width:150 ~height:50 ~fontSize:25 ~onClick:(fun _ -> dispatch Vider) ~children:[] ();
            Text.createElement ~text:""(*retour du moteur*) ~style:textStyle ~children:[] ()
            ] ()))
end

module History = struct
  let component =
    React.component "Historique"
  let createElement ?(hyst = [""]) =
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
      |e::l -> histol l ((Text.createElement ~text:e ~style:textStyle ~children:[]) :: res)
    in
    fun ~children:_ () ->
      component
        (fun hooks ->
          hooks, View.createElement ~style:containerStyle ~children:(histol hyst []) ())
end

type view =
  {
  name: string;
  render: Window.t -> React.syntheticElement;
  }

type state = {
  views: view list;
  selectedView: string;}

let state: state =
  {
    views =
      [{
         name = "Équation";
         render = ((fun _w  -> VuePrincipal.createElement ~children:[] ()));
       };
       {
         name = "Historique";
         render = ((fun _w -> History.createElement ~children:[] ()));
       }];
    selectedView = "Équation"
  }

let getViewByName (state : state) (view : string) =
  (List.filter (fun x  -> String.equal x.name view) state.views) |>
    List.hd

let getRenderFunctionSelector: state -> Window.t -> React.syntheticElement =
  fun (s : state)  ->
    (getViewByName s s.selectedView) |> (fun a  -> a.render)

type action =
  | SelectView of string

let reducer (action : action) (state : state) =
  match action with
  | ((SelectView (name))[@explicit_arity ]) -> { state with selectedView = name }

module Affichage = struct
  let component = React.component "Affichage"
  let createElement ~children:_ ~win () =
  component
  (fun hooks  ->
     let (state,dispatch,hooks) =
       React.Hooks.reducer ~initialState:state reducer hooks in
     let renderButton (x : view) =
       ((Button.createElement ~title:(x.name)
            ~width:175
            ~fontSize:25
            ~onClick:(fun _  ->
                       dispatch
                         ((SelectView ((x.name)))[@explicit_arity ]))
            ~children:[] ())[@JSX ]) in
     let buttons = List.map renderButton state.views in
     let viewRender = getRenderFunctionSelector state in
     let view = viewRender win in
     let bgColor = Color.hex("#212733") in
     let activeBackgroundColor = Color.hex("#2E3440") in
     (hooks,
       ((View.createElement ~onMouseWheel:(fun _evt  -> ())
     ~style:(let open Style in
               [position `Absolute;
               justifyContent `Center;
               alignItems `Center;
               backgroundColor bgColor;
               bottom 0;
               top 0;
               left 0;
               right 0;
               flexDirection `Row])
     ~children:[((ScrollView.createElement
                    ~style:(let open Style in
                              [position `Absolute;
                              top 0;
                              left 0;
                              width 175;
                              bottom 0;
                              backgroundColor bgColor])
                    ~children:[((View.createElement ~children:buttons ())
                              [@JSX ])] ())[@JSX ]);
               ((View.createElement
                   ~style:(let open Style in
                             [position `Absolute;
                             top 0;
                             left 175;
                             right 0;
                             bottom 0;
                             backgroundColor activeBackgroundColor])
                   ~children:[view] ())[@JSX ])] ())[@JSX ])))
  end

let init app =
  let fen =
  App.createWindow ~createOptions:(WindowCreateOptions.create ~icon:((Some ("logo.png"))[@explicit_arity ]) ()) app "OCalc"
  in

  let afficher () = Affichage.createElement ~win:fen ~children:[] () in
  UI.start fen (afficher ()) (afficher ())

let _ = App.start init
