let input_path = "input/input01.txt"

let input =
  let ic = open_in_bin input_path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let first_digit_seq seq =
  let digit_of_char ch =
    match ch with
    | '0' .. '9' -> Some (Char.code ch - Char.code '0')
    | _ -> None
  in
  Seq.find_map digit_of_char seq |> Option.get

let first_digit line = first_digit_seq (String.to_seq line)

let last_digit line =
  let len = String.length line in
  first_digit_seq (Seq.init len (fun n -> String.get line (len - n - 1)))

let () =
  String.trim input |> String.split_on_char '\n'
  |> List.map (fun line -> (first_digit line * 10) + last_digit line)
  |> List.fold_left ( + ) 0 |> Printf.printf "%d\n"

let digit_of_string =
  List.append
    (List.init 9 (fun n -> (string_of_int (n + 1), n + 1)))
    [
      ("one", 1);
      ("two", 2);
      ("three", 3);
      ("four", 4);
      ("five", 5);
      ("six", 6);
      ("seven", 7);
      ("eight", 8);
      ("nine", 9);
    ]

let first_real_digit line =
  let prefix_digit s =
    Seq.find_map
      (fun (prefix, n) -> if String.starts_with ~prefix s then Some n else None)
      (List.to_seq digit_of_string)
  in
  let string_seq =
    let len = String.length line in
    Seq.init len (fun n -> String.sub line n (len - n))
  in
  Seq.find_map prefix_digit string_seq |> Option.get

let last_real_digit line =
  let suffix_digit s =
    Seq.find_map
      (fun (suffix, n) -> if String.ends_with ~suffix s then Some n else None)
      (List.to_seq digit_of_string)
  in
  let string_seq =
    let len = String.length line in
    Seq.init len (fun n -> String.sub line 0 (len - n))
  in
  Seq.find_map suffix_digit string_seq |> Option.get

let () =
  String.trim input |> String.split_on_char '\n'
  |> List.map (fun line -> (first_real_digit line * 10) + last_real_digit line)
  |> List.fold_left ( + ) 0 |> Printf.printf "%d\n"
