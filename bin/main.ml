[@@@warning "-A"]

type cell =
  | Camel
  | Empty

type model = {
  size: int;
  camels: int;
  grid: cell array array;
  revealed: bool array array;
  generated: bool;
}

type msg =
  | Clicked of int * int
  | Calculate of int * int
  | Generate of int * int
  | Game_over

let init =
  let size = 8 in
  let grid = Array.init size (fun _ -> Array.make size Empty) in
  let revealed = Array.init size (fun _ -> Array.make size false) in
  Vdom.return
    {
      size;
      camels = 10;
      grid;
      revealed;
      generated = false;
    }


let print_revealed a =
  Array.iter (fun row -> Array.iter (fun x -> print_string @@ string_of_bool x ^ " ") row; print_newline ()) a

let print_grid grid =
  Array.iter (fun row -> Array.iter (fun c -> print_string @@ (if c = Camel then "C" else "E") ^ " ") row; print_newline ()) grid

let print_list = List.iter (fun x -> Printf.printf "%d " x)

let camel_string = "🐪"

let generate grid camels size first =
  let n = size * size in
  Random.self_init ();
  let dummy_sort = List.fast_sort (fun _ _ -> -1 + Random.int 3) in
  let camels_positions =
    Seq.ints 0 |> Seq.take n |> Seq.filter ((!=) first) |> List.of_seq
    |> dummy_sort |> dummy_sort
    |> List.to_seq |> Seq.take camels |> List.of_seq
  in
  List.iter (fun pos ->
      let i = pos / size in
      let j = pos mod size in
      grid.(i).(j) <- Camel
    )
    camels_positions

let update ({camels; size; grid; revealed; generated;} as model) = function
  | Clicked (i, j) ->
    Printf.printf "(%d, %d)\n" i j;
    revealed.(i).(j) <- true;
    let c = [Vdom.Cmd.echo (Calculate (i, j))] in
    if not generated then
      Vdom.return ~c:[Vdom.Cmd.batch (Vdom.Cmd.echo (Generate (i, j)) :: c)] model
    else
      Vdom.return ~c model
  | Calculate (i, j) ->
    if grid.(i).(j) = Camel then
      Vdom.return ~c:[Vdom.Cmd.echo Game_over] model
    else
      Vdom.return model
  | Generate (i, j) ->
    print_endline "Generating the grid!";
    generate grid camels size (i * size + j);
    print_grid grid;
    Vdom.return {model with generated = true}
  | Game_over ->
    Js_browser.Window.alert Js_browser.window "Game Over!";
    Vdom.return model

let is_revealed model (i, j) = model.revealed.(i).(j)
let is_camel model (i, j) = model.grid.(i).(j) = Camel

let button ~revealed ~camel (i, j) =
  let s = Vdom.style in
  Vdom.elt "button"
    ~a:[
      Vdom.type_button;
      Vdom.onclick (fun _ -> Clicked (i, j));
      s"margin" "1.5px";
      s"width" "40px";
      s"height" "40px";
      s"min-height" "40px";
      s"min-width" "40px";
      Vdom.disabled revealed;
    ]
    [Vdom.text (if revealed && camel then camel_string else "O")]


let view ({grid; _} as model) =
  let buttons =
    Array.mapi (fun i row ->
        Vdom.div (
          Array.mapi (fun j cell ->
              let revealed = is_revealed model (i, j) in
              button ~revealed ~camel:(is_camel model (i,j)) (i, j)
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
