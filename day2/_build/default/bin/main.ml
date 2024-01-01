let read_file filename =
  try
    let ic = open_in filename in
    let rec read_lines acc =
      try
        let line = input_line ic in
        read_lines (line :: acc)
      with End_of_file -> List.rev acc
    in
    let lines = read_lines [] in
    close_in ic;
    Some lines
  with Sys_error msg ->
    prerr_endline ("Error: " ^ msg);
    None

module CubeAmounts = Map.Make (String)

let validCubeAmounts = CubeAmounts.add "red" 12 CubeAmounts.empty

let validCubeAmounts =
  validCubeAmounts |> CubeAmounts.add "green" 13 |> CubeAmounts.add "blue" 14

let input = match read_file "input.txt" with None -> exit 1 | Some x -> x

let split_string_into_parts seperator s =
  match String.split_on_char seperator s with
  | [ x; y ] -> (x, y)
  | [ x ] -> (x, "")
  | _ -> ("", "")

let parse_selection selection =
  let amount_str, cube = split_string_into_parts ' ' selection in
  let amount = amount_str |> int_of_string in
  (cube, amount)

let invalid_selection selection =
  let cube, amount = selection |> parse_selection in
  validCubeAmounts |> CubeAmounts.find cube < amount

let parse_selections round =
  round |> String.split_on_char ',' |> List.map String.trim

let invalid_round round =
  round |> parse_selections
  |> List.exists (fun selection ->
         match selection with "" -> false | _ -> invalid_selection selection)

let parse_game_parts game_line =
  let game_str, rest_of_game = split_string_into_parts ':' game_line in
  let _, game_number_str = game_str |> split_string_into_parts ' ' in
  let game_number = game_number_str |> int_of_string in
  let rounds =
    rest_of_game |> String.split_on_char ';' |> List.map String.trim
  in
  (game_number, rounds)

let validate_game game_line =
  let game_number, rounds = game_line |> parse_game_parts in
  if
    rounds
    |> List.exists (fun round ->
           match round with "" -> false | _ -> invalid_round round)
  then 0
  else game_number

let validate_games game_lines =
  game_lines |> List.fold_left (fun prev next -> prev + validate_game next) 0

let () =
  print_endline
    ("Answer to part one is " ^ (input |> validate_games |> string_of_int))

let rec process_selections minimum selections =
  match selections with
  | selection :: rest ->
      let cube, amount = selection |> parse_selection in
      let minimum =
        minimum
        |> CubeAmounts.update cube (fun v ->
               match v with
               | Some x -> if x > amount then Some x else Some amount
               | None -> Some amount)
      in
      process_selections minimum rest
  | [] -> minimum

let rec process_rounds minimum rounds =
  match rounds with
  | round :: rest ->
      let selections = round |> parse_selections in
      let minimum = selections |> process_selections minimum in
      process_rounds minimum rest
  | [] -> minimum

let process_games game_lines =
  let _, rounds = game_lines |> parse_game_parts in
  let minimum = CubeAmounts.empty in
  let minimum =
    minimum |> CubeAmounts.add "blue" 0 |> CubeAmounts.add "green" 0
    |> CubeAmounts.add "red" 0
  in
  rounds |> process_rounds minimum

let power_set_minimums minimum =
  1 |> CubeAmounts.fold (fun _ value product -> value * product) minimum

let print_map map =
  map
  |> CubeAmounts.iter (fun cube amount ->
         print_endline (cube ^ ": " ^ (amount |> string_of_int)))

let () =
  print_endline
    ("Answer to part two is "
    ^ (input
      |> List.fold_left
           (fun prev next ->
             let minimum = process_games next in
             minimum |> print_map;
             (minimum |> power_set_minimums) + prev)
           0
      |> string_of_int))
