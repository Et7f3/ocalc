open Revery
open Revery.Math
open Revery.UI
open Revery.UI.Components
module SimpleButton =
  struct
    let component =
      React.component (("SimpleButton")[@reason.raw_literal "SimpleButton"])
    let createElement ~children:_ ?(click = fun () -> ()) () =
      component
        (fun hooks  ->
           let (count,setCount,hooks) = React.Hooks.state 0 hooks in
           let wrapperStyle =
             let open Style in
               [backgroundColor (Color.rgba 0. 0. 0. 0.1);
               border ~width:2 ~color:Colors.white;
               margin 10] in
           let textHeaderStyle =
             let open Style in
               [color Colors.red;
               fontFamily
                 (("Roboto-Regular.ttf")[@reason.raw_literal
                                          "Roboto-Regular.ttf"]);
               fontSize 20;
               margin 3] in
           let textContent =
             (("Rechercher")[@reason.raw_literal "Rechercher"]) in
           (hooks,
             ((Clickable.createElement ~onClick:click
                 ~children:[((View.createElement ~style:wrapperStyle
                                ~children:[((Text.createElement
                                               ~style:textHeaderStyle
                                               ~text:textContent ~children:[]
                                               ())[@JSX ])] ())[@JSX ])] ())
             [@JSX ])))
  end
module Entry =
  struct
    type inputFields = {
      first: string;
      second: string;}
    let component = React.component "Entry"
    let createElement ~children:_  () =
      component
        (fun hooks  ->
           let ({ first; second },setValue,hooks) =
             React.Hooks.state { first = ""; second = "" } hooks in
           (hooks,
             ((Input.createElement ~placeholder:"Entrez votre équation"
                 ~onChange:(fun { value;_}  ->
                              setValue { first = value; second })
                 ~children:[] ())[@JSX ])))
  end
let init app =
  let win =
    App.createWindow app
      (("OCalc")[@reason.raw_literal "OCalc"]) in
  let containerStyle =
    let open Style in
      [position `Absolute;
      justifyContent `Center;
      alignItems `Center;
      bottom 0;
      top 0;
      left 0;
      right 0] in
  let textstyle =
    let open Style in
      [color Colors.white;
      fontFamily
        (("Roboto-Regular.ttf")[@reason.raw_literal
                                 "Roboto-Regular.ttf"])] in
  let render () =
    let ent = (Entry.createElement ~children:[] ())[@JSX ] in
    let texte = (Text.createElement ~style:textstyle ~text:"" ~children:[] ())[@JSX ] in
    let button = (SimpleButton.createElement ~children:[] ())[@JSX ] in
    ((View.createElement ~style:containerStyle
        ~children:[ent; texte; button] ())
    [@JSX ]) in
  UI.start win render
let _ = App.start init
