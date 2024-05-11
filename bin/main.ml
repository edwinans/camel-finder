(* MIT License                     *)
(* Copyright (c) 2024 Edwin Ansari *)

type cell =
  | Camel
  | Empty of int

type cell_state =
  | Raw
  | Revealed
  | Flagged

type level =
  | Beginner
  | Intermediate
  | Expert

type model = {
  size: int;
  camels: int;
  flagged_camels: int;
  grid: cell array array;
  state: cell_state array array;
  generated: bool;
  finished: bool;
  level: level;
}

type msg =
  | Click of int * int
  | Toggle_flag of int * int
  | Calculate of int * int
  | Generate of {first: int * int}
  | Change_level of level
  | Reveal_all
  | Check_win
  | Won
  | Game_over


module Utils = struct
  [@@@warning "-32"]

  let print_revealed a =
    Array.iter (fun row -> Array.iter (fun x -> print_string @@ string_of_bool (x = Revealed) ^ " ") row; print_newline ()) a

  let print_grid grid =
    Array.iter (fun row -> Array.iter (fun c -> print_string @@ (if c = Camel then "C" else "E") ^ " ") row; print_newline ()) grid

  let print_ints = List.iter (fun x -> Printf.printf "%d " x)

  let log_cell f (i, j) =
    Printf.printf "%s (%d, %d)\n" f i j;
end

let config_of_level = function
  | Beginner -> 8, 10
  | Intermediate -> 16, 40
  | Expert -> 25, 99

let init level =
  let size, camels = config_of_level level in
  let grid = Array.init size (fun _ -> Array.make size (Empty 0)) in
  let state = Array.init size (fun _ -> Array.make size Raw) in
  Vdom.return
    {
      size;
      camels;
      flagged_camels = 0;
      grid;
      state;
      generated = false;
      finished = false;
      level;
    }

let camel_string = "🐪"
let cactus_string = "🌵"
let invisible_string = "\u{200E}"

let is_raw model (i, j) = model.state.(i).(j) = Raw
let is_revealed model (i, j) = model.state.(i).(j) = Revealed
let is_camel model (i, j) = model.grid.(i).(j) = Camel

let toggle_cell_flag ({state; flagged_camels; _} as model) (i, j) =
  let st, diff =
    match state.(i).(j) with
    | Raw -> Flagged, +1
    | Flagged -> Raw, -1
    | Revealed -> Revealed, 0
  in
  state.(i).(j) <- st;
  {model with flagged_camels = flagged_camels + diff}

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

let check_win {grid; state; camels; flagged_camels; generated; _} =
  match generated && camels = flagged_camels with
  | false -> false
  | true ->
    Array.mapi (fun i ->
        Array.mapi (fun j -> function
            | Camel -> state.(i).(j) = Flagged
            | Empty _ -> state.(i).(j) <> Flagged
          )
      )
      grid
    |> Array.for_all (Array.for_all Fun.id)

let update ({camels; size; grid; state; generated; _} as model) = function
  | Click (i, j) ->
    Utils.log_cell "click" (i, j);
    state.(i).(j) <- Revealed;
    let c = [Vdom.Cmd.echo (Calculate (i, j))] in
    if not generated then
      Vdom.return ~c:[Vdom.Cmd.batch (Vdom.Cmd.echo (Generate {first = (i, j)}) :: c)] model
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
    let model = toggle_cell_flag model (i, j) in
    Vdom.return ~c:[Vdom.Cmd.echo Check_win] model
  | Generate {first} ->
    print_endline "Generating the grid!";
    generate_grid ~grid ~camels ~size ~first;
    Utils.print_grid grid;
    Vdom.return {model with generated = true}
  | Change_level level ->
    print_endline "change_level";
    init level
  | Reveal_all ->
    let state = Array.map (Array.map (fun _ -> Revealed)) state in
    Vdom.return {model with state}
  | Check_win ->
    let c =
      match check_win model with
      | true -> Some [Vdom.Cmd.echo Won]
      | false -> None
    in
    Vdom.return ?c model
  | Won ->
    Js_browser.Window.alert Js_browser.window "WON 🏜!";
    Vdom.return {model with finished = true}
  | Game_over ->
    Js_browser.Window.alert Js_browser.window "Game Over!";
    Vdom.return ~c:[Vdom.Cmd.echo Reveal_all] {model with finished = true}


let s = Vdom.style

let button model (i, j) =
  let txt =
    match model.grid.(i).(j), model.state.(i).(j) with
    | Camel, Revealed -> camel_string
    | Empty nb, Revealed -> string_of_int nb
    | _, Flagged -> cactus_string
    | _ -> invisible_string
  in
  Vdom.elt "button"
    ~a:[
      Vdom.type_button;
      Vdom.onclick (fun _ -> Click (i, j));
      Vdom.oncontextmenu (fun _ -> Toggle_flag (i, j));
      s"margin" "1.5px";
      s"width" "40px";
      s"height" "40px";
      s"min-height" "40px";
      s"min-width" "40px";
      Vdom.disabled (model.finished || is_revealed model (i, j));
    ]
    [Vdom.text txt]

let status {camels; flagged_camels; _} =
  Vdom.elt "p"
    ~a:[
      s"margin-top" "1%";
    ]
    [Vdom.text
       (Printf.sprintf "%s / %s = %d / %d" cactus_string camel_string flagged_camels camels)
    ]

let level_picker {level; _} =
  let levels = ["Beginner"; "Intermediate"; "Expert";] in
  let current_level =
    List.nth levels
      (match level with | Beginner -> 0 | Intermediate -> 1 | Expert -> 2)
  in
  let level_of_string s =
    assert (List.mem s levels);
    match s with
    | "Beginner" -> Beginner
    | "Intermediate" -> Intermediate
    | "Expert" -> Expert
    | _ -> assert false
  in
  let option v = Vdom.elt ~a:[Vdom.value v] "option" [Vdom.text v] in
  Vdom.elt "select"
    ~a:[
      Vdom.value current_level;
      Vdom.onchange (fun s -> Change_level (level_of_string s));
    ]
    (List.map option levels)

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
  Vdom.div [
    Vdom.div buttons;
    status model;
    level_picker model;
  ]


let _ =
  let app = Vdom.app ~init:(init Beginner) ~update ~view () in
  let container = Js_browser.Document.body Js_browser.document in
  Vdom_blit.dom (Vdom_blit.run ~container app)
