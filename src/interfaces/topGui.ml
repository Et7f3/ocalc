open Revery
open Revery.UI
open Revery.UI.Components

let init app =
  let win =
  App.createWindow app (("OCalc")[@reason.raw_literal "OCalc"])
  in
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
  let button = Button.createElement ~onClick:(fun _ -> print_endline "cliked") ~title:"recherchez" ~children:[] () in
  let render () = View.createElement ~style:containerStyle ~children:[button] () in
  UI.start win render

let _ = App.start init
