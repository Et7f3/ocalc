open Revery
open Revery_UI
open Revery_Core

let init app =
  let win = App.createWindow app (("test")[@reason.raw_literal "test"]) in
  let textHeaderStyle =
    let open Style in
      [backgroundColor Colors.black;
      color Colors.white;
      fontFamily
        (("Roboto-Regular.ttf")[@reason.raw_literal "Roboto-Regular.ttf"]);
      fontSize 24] in
  let render () =
    ((View.createElement
        ~style:(let open Style in
                  [position `Absolute;
                  bottom 10;
                  top 10;
                  left 10;
                  right 10;
                  backgroundColor Colors.blue])
        ~children:[((View.createElement
                       ~style:(let open Style in
                                 [position `Absolute;
                                 bottom 0;
                                 width 10;
                                 height 10;
                                 backgroundColor Colors.red]) ~children:[] ())
                  [@JSX ]);
                  ((Image.createElement ~src:(("logo.png")[@reason.raw_literal "logo.png"])
                      ~style:Style.[width 128; height 64]
                      ~children:[] ())[@JSX ]);
                  ((Text.createElement ~style:textHeaderStyle
                      ~children:[] ())
                  [@JSX ]);
                  ((View.createElement
                      ~style:(let open Style in
                                [width 25;
                                height 25;
                                backgroundColor Colors.green]) ~children:[]
                      ())[@JSX ])] ())[@JSX ]) in
  UI.start win render[@@ocaml.doc "\n * The 'main' function for our app.\n "]
let _ = App.start init
