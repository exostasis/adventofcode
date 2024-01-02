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

let input = match read_file "input.txt" with Some x -> x | _ -> exit 1

type symbol = { x : int; y : int; symbol : char }
type part_number = { start_x : int; end_x : int; y : int; value : int }

(* let symbol_to_string symbol = *)
(*   "(x, y) -> (" ^ string_of_int symbol.x ^ ", " ^ string_of_int symbol.y ^ ")" *)

(* let part_number_to_string part_number_optional = *)
(*   match part_number_optional with *)
(*   | Some part_number -> *)
(*       "(x0-x1, y, value) -> (" *)
(*       ^ string_of_int part_number.start_x *)
(*       ^ "-" *)
(*       ^ string_of_int part_number.end_x *)
(*       ^ ", " *)
(*       ^ string_of_int part_number.y *)
(*       ^ ", " *)
(*       ^ string_of_int part_number.value *)
(*       ^ ")" *)
(*   | None -> "" *)

let rec find_int_end row x =
  if x = String.length row then x
  else
    match row.[x] with
    | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
        find_int_end row (x + 1)
    | _ -> x

let write_part_number part_numbers part_number =
  let x_indexes =
    List.init (part_number.end_x - part_number.start_x) (fun i ->
        i + part_number.start_x)
  in
  x_indexes
  |> List.iter (fun x ->
         let new_y = part_number.y in
         part_numbers.(x).(new_y) <- Some part_number)

let clear_part_number part_numbers part_number =
  let x_indexes =
    List.init (part_number.end_x - part_number.start_x) (fun i ->
        i + part_number.start_x)
  in
  x_indexes
  |> List.iter (fun x ->
         let new_y = part_number.y in
         part_numbers.(x).(new_y) <- None)

let rec process_row row symbols part_numbers x y =
  let length = String.length row in
  if x >= length then (symbols, part_numbers)
  else
    match row.[x] with
    | '.' -> process_row row symbols part_numbers (x + 1) y
    | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
        let next_dot_x = find_int_end row x in
        let value_str = String.sub row x (next_dot_x - x) in
        let value = value_str |> int_of_string in
        let part_number = { start_x = x; end_x = next_dot_x; y; value } in
        let () = write_part_number part_numbers part_number in
        process_row row symbols part_numbers next_dot_x y
    | symbol ->
        let new_symbols = symbols @ [ { x; y; symbol } ] in
        process_row row new_symbols part_numbers (x + 1) y

let rec process_input input symbols part_numbers y =
  match input with
  | row :: rest ->
      let new_symbols, new_part_numbers =
        process_row row symbols part_numbers 0 y
      in
      process_input rest new_symbols new_part_numbers (y + 1)
  | [] -> (symbols, part_numbers)

(* let print_symbol_list list = *)
(*   list |> List.iter (fun x -> x |> symbol_to_string |> print_endline) *)

(* let print_part_number_list list = *)
(*   list |> List.iter (fun x -> x |> part_number_to_string |> print_endline) *)

(* let print_part_numbers list = *)
(*   list *)
(*   |> Array.iter (fun row -> *)
(*          row *)
(*          |> Array.iteri (fun i col -> *)
(*                 match col with *)
(*                 | None -> print_char '.' *)
(*                 | Some part_number -> *)
(*                     print_char *)
(*                       (part_number.value |> string_of_int).[i *)
(*                                                             - part_number *)
(*                                                                 .start_x]); *)
(*          print_endline "") *)

let valid_cord part_numbers x y =
  x >= 0
  && x < Array.length part_numbers
  && y >= 0
  && y < Array.length part_numbers.(0)

let filter_invalid_indexes symbol part_numbers indexes =
  indexes
  |> List.filter (fun (x_inc, y_inc) ->
         let new_x = symbol.x + x_inc in
         let new_y = symbol.y + y_inc in
         valid_cord part_numbers new_x new_y)

let find_part_numbers symbol part_numbers =
  let indexes =
    [ (-1, -1); (0, -1); (1, -1); (-1, 0); (1, 0); (-1, 1); (0, 1); (1, 1) ]
  in
  let indexes = indexes |> filter_invalid_indexes symbol part_numbers in
  indexes
  |> List.fold_left
       (fun prev (x_inc, y_inc) ->
         let new_x = symbol.x + x_inc in
         let new_y = symbol.y + y_inc in
         match part_numbers.(new_x).(new_y) with
         | Some part_number ->
             let () = clear_part_number part_numbers part_number in
             prev + part_number.value
         | None -> prev)
       0

let rec process_part_number_data symbols part_numbers sum =
  match symbols with
  | symbol :: rest_symbols ->
      let new_sum = find_part_numbers symbol part_numbers in
      process_part_number_data rest_symbols part_numbers (sum + new_sum)
  | [] -> sum

let height = List.length input
let width = List.nth input 0 |> String.length

let symbols, part_numbers =
  process_input input [] (Array.make_matrix width height None) 0

let sum = process_part_number_data symbols part_numbers 0
let () = print_endline ("Part one results " ^ string_of_int sum)

let unique list =
  let rec aux list acc =
    match list with
    | [] -> acc
    | x :: rest ->
        if List.mem x acc then aux rest acc else aux rest (acc @ [ x ])
  in
  aux list []

let gear_ratio_parts symbol part_numbers =
  let indexes =
    [ (-1, -1); (0, -1); (1, -1); (-1, 0); (1, 0); (-1, 1); (0, 1); (1, 1) ]
  in
  let indexes = indexes |> filter_invalid_indexes symbol part_numbers in
  let found_part_numbers =
    indexes
    |> List.map (fun (x_inc, y_inc) ->
           let new_x = symbol.x + x_inc in
           let new_y = symbol.y + y_inc in
           part_numbers.(new_x).(new_y))
  in
  (* let () = print_part_number_list found_part_numbers in *)
  let actual_part_numbers =
    found_part_numbers
    |> List.filter (fun p -> match p with Some _ -> true | None -> false)
  in
  (* let () = print_endline (List.length actual_part_numbers |> string_of_int) in *)
  let actual_part_numbers =
    actual_part_numbers
    |> List.map (fun x -> match x with Some v -> v | None -> assert false)
  in
  (* let () = *)
  (*   actual_part_numbers *)
  (*   |> List.iter (fun p -> print_endline (string_of_int p.value)) *)
  (* in *)
  unique actual_part_numbers

let rec process_gear_ratio_data symbols part_numbers sum =
  (* let () = *)
  (*   symbols *)
  (*   |> List.iter (fun symbol -> *)
  (*          print_endline *)
  (*            ("(" ^ string_of_int symbol.x ^ "," ^ string_of_int symbol.y *)
  (*           ^ "): " *)
  (*            ^ String.make 1 symbol.symbol)) *)
  (* in *)
  match symbols with
  | gear_ratio_symbol :: rest_of_symbols -> (
      match gear_ratio_symbol with
      | { x = _; y = _; symbol } when symbol = '*' ->
          let gear_parts = gear_ratio_parts gear_ratio_symbol part_numbers in
          (* let () = *)
          (*   print_endline *)
          (*     ("Gear parts has " ^ string_of_int (List.length gear_parts)) *)
          (* in *)
          let gear_ratio =
            gear_parts
            |> List.fold_left
                 (fun prev part_number -> prev * part_number.value)
                 1
          in
          (* let () = *)
          (*   print_endline ("Gear ratio is " ^ string_of_int gear_ratio) *)
          (* in *)
          let new_sum =
            if List.length gear_parts = 2 then sum + gear_ratio else sum
          in
          process_gear_ratio_data rest_of_symbols part_numbers new_sum
      | _ -> process_gear_ratio_data rest_of_symbols part_numbers sum)
  | [] -> sum

let symbols, part_numbers =
  process_input input [] (Array.make_matrix width height None) 0

let sum = process_gear_ratio_data symbols part_numbers 0
let () = print_endline ("Part two results " ^ string_of_int sum)
