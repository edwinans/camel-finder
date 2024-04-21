[@@@warning "-A"]

type cell =
  | Camel
  | Empty

type model = {
  size: int;
  grid: cell array array;
  state: bool array array; (* revealed or not *)
}

type msg =
  | Clicked of int * int

let init: model =
  let size = 10 in
  let grid = Array.make size (Array.make size Empty) in
  let state = Array.make size (Array.make size false) in
  {
    size;
    grid;
    state;
  }

let update model = function
  | Clicked (i, j) -> Printf.printf "(%d, %d)\n" i j; model

let view _model =
  Vdom.div [
    Vdom.elt "button" ~a:[Vdom.type_button; Vdom.onclick (fun _ -> Clicked (0,0))]
      [Vdom.text "x"];
  ]

let _ =
  let app = Vdom.simple_app ~init ~update ~view () in
  let container = Js_browser.Document.body Js_browser.document in
  Vdom_blit.dom (Vdom_blit.run ~container app)
