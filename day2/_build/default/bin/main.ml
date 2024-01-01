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

let input = match read_file "test.txt" with None -> exit 1 | Some x -> x

let split_string_into_parts seperator s =
  match String.split_on_char seperator s with
  | [ x; y ] -> (x, y)
  | [ x ] -> (x, "")
  | _ -> ("", "")

let invalid_selection selection =
  let amount_str, cube = split_string_into_parts ' ' selection in
  let amount = amount_str |> int_of_string in
  validCubeAmounts |> CubeAmounts.find cube < amount

let invalid_round round =
  round |> String.split_on_char ',' |> List.map String.trim
  |> List.exists (fun selection ->
         match selection with "" -> false | _ -> invalid_selection selection)

let validate_game game_line =
  let game_str, rest_of_game = split_string_into_parts ':' game_line in
  let _, game_number_str = game_str |> split_string_into_parts ' ' in
  let game_number = game_number_str |> int_of_string in
  let rounds =
    rest_of_game |> String.split_on_char ';' |> List.map String.trim
  in
  if
    rounds
    |> List.exists (fun round ->
           match round with "" -> false | _ -> invalid_round round)
  then 0
  else game_number

let validate_games game_lines =
  game_lines |> List.fold_left (fun prev next -> prev + validate_game next) 0

let () = input |> validate_games |> string_of_int |> print_endline
