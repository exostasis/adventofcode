exception NoIntCharFound

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

let get_first_int input =
  let rec find_int index =
    if index = String.length input then (
      print_endline input;
      raise NoIntCharFound)
    else if
      Char.code input.[index] >= Char.code '0'
      && Char.code input.[index] <= Char.code '9'
    then String.sub input index 1
    else find_int (index + 1)
  in
  find_int 0

let get_last_int input =
  let rec find_int index =
    if index < 0 then (
      print_endline input;
      raise NoIntCharFound)
    else if
      Char.code input.[index] >= Char.code '0'
      && Char.code input.[index] <= Char.code '9'
    then String.sub input index 1
    else find_int (index - 1)
  in
  find_int (String.length input - 1)

let convert_input_to_numbers list =
  List.map (fun x -> int_of_string (get_first_int x ^ get_last_int x)) list

let input = read_file "input.txt"

(* let () = *)
(*   match input with *)
(*   | None -> exit 1 *)
(*   | Some x -> List.iter (fun x -> print_endline x) x *)

let numbers =
  match input with None -> exit 1 | Some x -> convert_input_to_numbers x

let sum = List.fold_left (fun prev next -> prev + next) 0 numbers
let () = print_endline ("Part one answer " ^ string_of_int sum)

let get_first_int input =
  let insensitive_string = String.lowercase_ascii input in
  (* let () = print_endline insensitive_string in *)
  let rec find_int index =
    if index = String.length insensitive_string then (
      print_endline input;
      raise NoIntCharFound)
    else
      (* print_endline (String.sub insensitive_string index 1); *)
      match insensitive_string.[index] with
      | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
          String.sub input index 1
      | 'z' ->
          if
            index + 3 < String.length insensitive_string
            && String.sub insensitive_string index 4 = "zero"
          then "0"
          else find_int (index + 1)
      | 'o' ->
          if
            index + 2 < String.length insensitive_string
            && String.sub insensitive_string index 3 = "one"
          then "1"
          else find_int (index + 1)
      | 't' ->
          if
            index + 2 < String.length insensitive_string
            && String.sub insensitive_string index 3 = "two"
          then "2"
          else if
            index + 4 < String.length insensitive_string
            && String.sub insensitive_string index 5 = "three"
          then "3"
          else find_int (index + 1)
      | 'f' ->
          if index + 3 < String.length insensitive_string then
            match String.sub insensitive_string index 4 with
            | "four" -> "4"
            | "five" -> "5"
            | _ -> find_int (index + 1)
          else find_int (index + 1)
      | 's' ->
          if
            index + 2 < String.length insensitive_string
            && String.sub insensitive_string index 3 = "six"
          then "6"
          else if
            index + 4 < String.length insensitive_string
            && String.sub insensitive_string index 5 = "seven"
          then "7"
          else find_int (index + 1)
      | 'e' ->
          if
            index + 4 < String.length insensitive_string
            && String.sub insensitive_string index 5 = "eight"
          then "8"
          else find_int (index + 1)
      | 'n' ->
          if
            index + 3 < String.length insensitive_string
            && String.sub insensitive_string index 4 = "nine"
          then "9"
          else find_int (index + 1)
      | _ -> find_int (index + 1)
  in
  find_int 0

let get_last_int input =
  let insensitive_string = String.lowercase_ascii input in
  (* let () = print_endline insensitive_string in *)
  let rec find_int index =
    if index < 0 then (
      print_endline input;
      raise NoIntCharFound)
    else
      (* print_endline (String.sub insensitive_string index 1); *)
      match insensitive_string.[index] with
      | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
          String.sub input index 1
      | 'o' ->
          if
            index - 3 >= 0
            && String.sub insensitive_string (index - 3) 4 = "zero"
          then "0"
          else if
            index - 2 >= 0
            && String.sub insensitive_string (index - 2) 3 = "two"
          then "2"
          else find_int (index - 1)
      | 'e' ->
          if
            index - 2 >= 0
            && String.sub insensitive_string (index - 2) 3 = "one"
          then "1"
          else if
            index - 3 >= 0
            && (insensitive_string.[index - 1] = 'v'
               || insensitive_string.[index - 1] = 'n')
          then
            match String.sub insensitive_string (index - 3) 4 with
            | "nine" -> "9"
            | "five" -> "5"
            | _ -> find_int (index - 1)
          else if
            index - 4 >= 0
            && String.sub insensitive_string (index - 4) 5 = "three"
          then "3"
          else find_int (index - 1)
      | 'r' ->
          if
            index - 3 >= 0
            && String.sub insensitive_string (index - 3) 4 = "four"
          then "4"
          else find_int (index - 1)
      | 'x' ->
          if
            index - 2 >= 0
            && String.sub insensitive_string (index - 2) 3 = "six"
          then "6"
          else find_int (index - 1)
      | 'n' ->
          if
            index - 4 >= 0
            && String.sub insensitive_string (index - 4) 5 = "seven"
          then "7"
          else find_int (index - 1)
      | 't' ->
          if
            index - 4 >= 0
            && String.sub insensitive_string (index - 4) 5 = "eight"
          then "8"
          else find_int (index - 1)
      | _ -> find_int (index - 1)
  in
  find_int (String.length insensitive_string - 1)

let convert_input_to_numbers list =
  let strings = List.map (fun x -> get_first_int x ^ get_last_int x) list in
  List.map (fun x -> int_of_string x) strings

(* let () = *)
(*   let value = get_first_int "53hvhgchljnlxqjsgrhxgf1zfoureightmlhvvv" in *)
(*   print_endline ("First int value " ^ value) *)
(**)
(* let () = *)
(*   let value = get_last_int "53hvhgchljnlxqjsgrhxgf1zfoureightmlhvvv" in *)
(*   print_endline ("Last int value " ^ value) *)

let numbers =
  match input with None -> exit 1 | Some x -> convert_input_to_numbers x

let () = print_endline "Numbers list:"

let () =
  let someInput = match input with None -> exit 1 | Some x -> x in
  List.iter2
    (fun y x -> print_endline (y ^ ": " ^ string_of_int x))
    someInput numbers

let sum = List.fold_left (fun prev next -> prev + next) 0 numbers
let () = print_endline ("Part two answer " ^ string_of_int sum)
