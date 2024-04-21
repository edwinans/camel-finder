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
  | Game_over

let init =
  let size = 10 in
  let grid = Array.init size (fun _ -> Array.make size Empty) in
  let state = Array.init size (fun _ -> Array.make size false) in
  grid.(0).(1) <- Camel;
  Vdom.return
    {
      size;
      grid;
      state;
    }

let update ({grid; state; _} as model) = function
  | Clicked (i, j) ->
    Printf.printf "(%d, %d)\n" i j;
    state.(i).(j) <- true;
    if grid.(i).(j) = Camel then
      Vdom.return ~c:[Vdom.Cmd.echo Game_over] model
    else
      Vdom.return model
  | Game_over ->
    Js_browser.Window.alert Js_browser.window "Game Over!";
    Vdom.return model

let print_state a =
  Array.iter (fun row -> Array.iter (fun x -> print_string @@ string_of_bool x ^ " ") row; print_newline ()) a

let is_revealed model (i, j) = model.state.(i).(j)

let button ~revealed (i, j) =
  Vdom.elt "button"
    ~a:[
      Vdom.type_button;
      Vdom.onclick (fun _ -> Clicked (i, j));
      Vdom.style "margin" "1.5px";
      Vdom.style "width" "40px";
      Vdom.style "height" "40px";
      Vdom.disabled revealed;
    ]
    []

let view ({grid; _} as model) =
  let buttons =
    Array.mapi (fun i row ->
        Vdom.div (
          Array.mapi (fun j cell ->
              let revealed = is_revealed model (i, j) in
              button ~revealed (i, j)
            ) row
          |> Array.to_list
        )
      ) grid
    |> Array.to_list
  in
  Vdom.div
    buttons

let _ =
  let app = Vdom.app ~init ~update ~view () in
  let container = Js_browser.Document.body Js_browser.document in
  Vdom_blit.dom (Vdom_blit.run ~container app)
