let input_path = "input/input01.txt"

let input =
  let ic = open_in_bin input_path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let rec first_digit_in_seq seq =
  let is_digit ch = match ch with '0' .. '9' -> true | _ -> false in
  match seq () with
  | Seq.Nil -> failwith "digit not found"
  | Cons (c, cs) ->
      if is_digit c then int_of_string (String.make 1 c)
      else first_digit_in_seq cs

let first_digit line = first_digit_in_seq (String.to_seq line)

let last_digit line =
  let len = String.length line in
  first_digit_in_seq (Seq.init len (fun n -> String.get line (len - n - 1)))

let () =
  String.trim input |> String.split_on_char '\n'
  |> List.map (fun line -> (first_digit line * 10) + last_digit line)
  |> List.fold_left Int.add 0
  |> fun n ->
  print_int n;
  print_newline ()
