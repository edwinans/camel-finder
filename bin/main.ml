[@@@warning "-32"]

type cell =
  | Camel
  | Empty of int

type cell_state =
  | Raw
  | Revealed
  | Flagged

type model = {
  size: int;
  camels: int;
  grid: cell array array;
  state: cell_state array array;
  generated: bool;
}

type msg =
  | Click of int * int
  | Toggle_flag of int * int
  | Calculate of int * int
  | Generate of int * int
  | Game_over


module Utils = struct
  let print_revealed a =
    Array.iter (fun row -> Array.iter (fun x -> print_string @@ string_of_bool (x = Revealed) ^ " ") row; print_newline ()) a

  let print_grid grid =
    Array.iter (fun row -> Array.iter (fun c -> print_string @@ (if c = Camel then "C" else "E") ^ " ") row; print_newline ()) grid

  let print_ints = List.iter (fun x -> Printf.printf "%d " x)

  let log_cell f (i, j) =
    Printf.printf "%s (%d, %d)\n" f i j;
end

let init =
  let size = 8 in
  let grid = Array.init size (fun _ -> Array.make size (Empty 0)) in
  let state = Array.init size (fun _ -> Array.make size Raw) in
  Vdom.return
    {
      size;
      camels = 10;
      grid;
      state;
      generated = false;
    }

let camel_string = "🐪"
let is_raw model (i, j) = model.state.(i).(j) = Raw
let is_revealed model (i, j) = model.state.(i).(j) = Revealed
let is_camel model (i, j) = model.grid.(i).(j) = Camel

let toggle_cell_flag model (i, j) =
  model.state.(i).(j) <-
    match model.state.(i).(j) with
    | Raw -> Flagged
    | Flagged -> Raw
    | Revealed -> Revealed

let project_1d (i, j) size = i * size + j
let project_2d pos size = (pos / size, pos mod size)

let neighbours (i, j) size =
  List.filter
    (fun (x, y) -> x >= 0 && x < size && y >= 0 && y < size)
    [(i-1, j-1); (i-1, j); (i-1, j+1); (i, j-1); (i, j+1); (i+1, j-1); (i+1, j); (i+1, j+1)]

let generate_grid ~grid ~camels ~size ~first =
  let n = size * size in
  let exclusions =
    List.map (fun p -> project_1d p size) (first :: neighbours first size)
  in
  let non_excluded pos = not (List.mem pos exclusions) in
  Random.self_init ();
  let dummy_sort = List.fast_sort (fun _ _ -> -1 + Random.int 3) in
  let camels_positions =
    Seq.ints 0 |> Seq.take n |> Seq.filter non_excluded |> List.of_seq
    |> dummy_sort |> dummy_sort |> dummy_sort
    |> List.to_seq |> Seq.take camels |> List.of_seq
  in
  List.iter (fun pos ->
      let (i, j) = project_2d pos size in
      grid.(i).(j) <- Camel
    )
    camels_positions

let rec calculate_expansion ({grid; state; size; _} as model) (i, j) =
  if grid.(i).(j) <> Camel then begin
    Utils.log_cell "calculate_expansion" (i, j);
    state.(i).(j) <- Revealed;
    let nbrs = neighbours (i, j) size in
    let nb_camels =
      List.fold_left
        (fun n (x, y) -> n + if grid.(x).(y) = Camel then 1 else 0)
        0
        nbrs
    in
    if nb_camels = 0 then
      List.iter
        (fun p -> calculate_expansion model p)
        (List.filter (fun p -> is_raw model p) nbrs)
    else
      grid.(i).(j) <- Empty nb_camels
  end

let update ({camels; size; grid; state; generated;} as model) = function
  | Click (i, j) ->
    Utils.log_cell "click" (i, j);
    state.(i).(j) <- Revealed;
    let c = [Vdom.Cmd.echo (Calculate (i, j))] in
    if not generated then
      Vdom.return ~c:[Vdom.Cmd.batch (Vdom.Cmd.echo (Generate (i, j)) :: c)] model
    else
      Vdom.return ~c model
  | Calculate (i, j) ->
    if is_camel model (i, j) then
      Vdom.return ~c:[Vdom.Cmd.echo Game_over] model
    else begin
      calculate_expansion model (i, j);
      Vdom.return model
    end
  | Toggle_flag (i, j) ->
    Utils.log_cell "toggle_flag" (i, j);
    toggle_cell_flag model (i, j);
    Vdom.return model
  | Generate (i, j) ->
    print_endline "Generating the grid!";
    generate_grid ~grid ~camels ~size ~first:(i, j);
    Utils.print_grid grid;
    Vdom.return {model with generated = true}
  | Game_over ->
    Js_browser.Window.alert Js_browser.window "Game Over!";
    Vdom.return model


let button model (i, j) =
  let s = Vdom.style in
  let revealed = is_revealed model (i, j) in
  let txt =
    match model.grid.(i).(j) with
    | Camel when revealed -> camel_string
    | Empty nb when revealed -> string_of_int nb
    | _ -> "O"
  in
  Vdom.elt "button"
    ~a:[
      Vdom.type_button;
      Vdom.onclick (fun _ -> Click (i, j));
      Vdom.oncontextmenu (fun _ -> Toggle_flag (i,j));
      s"margin" "1.5px";
      s"width" "40px";
      s"height" "40px";
      s"min-height" "40px";
      s"min-width" "40px";
      Vdom.disabled revealed;
    ]
    [Vdom.text txt]

let view ({grid; _} as model) =
  let buttons =
    Array.mapi (fun i row ->
        Vdom.div (
          Array.mapi (fun j _cell ->
              button model (i, j)
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
