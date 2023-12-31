let input_path = "input/input02.txt"

let input =
  let ic = open_in_bin input_path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let parse_line line =
  let colon = String.index line ':' in
  let game_id =
    String.sub line 0 colon |> String.split_on_char ' ' |> fun l ->
    List.nth l 1 |> int_of_string
  in
  let grabs =
    String.sub line (colon + 1) (String.length line - colon - 1)
    |> String.split_on_char ';'
    |> List.map (fun s ->
           String.split_on_char ',' s
           |> List.map (fun s ->
                  let pair = String.trim s |> String.split_on_char ' ' in
                  (List.nth pair 1, List.nth pair 0 |> int_of_string)))
  in
  (game_id, grabs)

let possible (color, num) =
  match color with
  | "red" -> num <= 12
  | "green" -> num <= 13
  | "blue" -> num <= 14
  | _ -> raise (Invalid_argument color)

let () =
  input |> String.trim |> String.split_on_char '\n' |> List.map parse_line
  |> List.filter (fun (_game_id, grabs) ->
         List.flatten grabs |> List.for_all possible)
  |> List.map (fun (game_id, _grabs) -> game_id)
  |> List.fold_left ( + ) 0 |> Printf.printf "%d\n"

let min_require grabs color =
  let max a b = if a > b then a else b in
  grabs
  |> List.map (fun l -> List.assoc_opt color l |> Option.value ~default:0)
  |> List.fold_left max 0

let power grabs =
  [ "red"; "green"; "blue" ]
  |> List.map (min_require grabs)
  |> List.fold_left Int.mul 1

let () =
  input |> String.trim |> String.split_on_char '\n' |> List.map parse_line
  |> List.map (fun (_game_id, grabs) -> power grabs)
  |> List.fold_left ( + ) 0 |> Printf.printf "%d\n"
