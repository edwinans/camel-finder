[@@@warning "-32"]

type cell =
  | Camel
  | Empty of int

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


module Utils = struct
  let print_revealed a =
    Array.iter (fun row -> Array.iter (fun x -> print_string @@ string_of_bool x ^ " ") row; print_newline ()) a

  let print_grid grid =
    Array.iter (fun row -> Array.iter (fun c -> print_string @@ (if c = Camel then "C" else "E") ^ " ") row; print_newline ()) grid

  let print_list = List.iter (fun x -> Printf.printf "%d " x)
end

let init =
  let size = 8 in
  let grid = Array.init size (fun _ -> Array.make size (Empty 0)) in
  let revealed = Array.init size (fun _ -> Array.make size false) in
  Vdom.return
    {
      size;
      camels = 8;
      grid;
      revealed;
      generated = false;
    }

let camel_string = "🐪"
let is_revealed model (i, j) = model.revealed.(i).(j)
let is_camel model (i, j) = model.grid.(i).(j) = Camel

let project_1d (i, j) size = i * size + j
let project_2d pos size = (pos / size, pos mod size)

let generate_grid ~grid ~camels ~size ~first =
  let n = size * size in
  let first = project_1d first size in
  Random.self_init ();
  let dummy_sort = List.fast_sort (fun _ _ -> -1 + Random.int 3) in
  let camels_positions =
    Seq.ints 0 |> Seq.take n |> Seq.filter ((<>) first) |> List.of_seq
    |> dummy_sort |> dummy_sort
    |> List.to_seq |> Seq.take camels |> List.of_seq
  in
  List.iter (fun pos ->
      let (i, j) = project_2d pos size in
      grid.(i).(j) <- Camel
    )
    camels_positions

let rec calculate_expansion ({grid; revealed; size; _} as model) (i, j) =
  if grid.(i).(j) <> Camel then begin
    Printf.printf "calculate (%d, %d)\n" i j;
    revealed.(i).(j) <- true;
    let neighbours =
      List.filter
        (fun (x, y) -> x >= 0 && x < size && y >= 0 && y < size)
        [(i-1, j-1); (i-1, j); (i-1, j+1); (i, j-1); (i, j+1); (i+1, j-1); (i+1, j); (i+1, j+1)]
    in
    let nb_camels =
      List.fold_left
        (fun n (x, y) -> n + if grid.(x).(y) = Camel then 1 else 0)
        0
        neighbours
    in
    if nb_camels = 0 then
      List.iter
        (fun (x, y) -> calculate_expansion model (x, y))
        (List.filter (fun (x, y) -> not revealed.(x).(y)) neighbours)
    else
      grid.(i).(j) <- Empty nb_camels
  end

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
    if is_camel model (i, j) then
      Vdom.return ~c:[Vdom.Cmd.echo Game_over] model
    else begin
      calculate_expansion model (i, j);
      Vdom.return model
    end
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
      Vdom.onclick (fun _ -> Clicked (i, j));
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
