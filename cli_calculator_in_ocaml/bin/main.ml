type number =
  | Int of int
  | Float of float

let is_int s =
  try ignore (int_of_string s); true with Failure _ -> false

let parse_number s =
  if is_int s then Int (int_of_string s)
  else Float (float_of_string s)

let calculate_integers operator first_number second_number =
  match operator with
    | '+' -> Printf.printf "%d %c %d = %d\n" first_number operator second_number (first_number + second_number)
    | '-' -> Printf.printf "%d %c %d = %d\n" first_number operator second_number (first_number - second_number)
    | '/' -> Printf.printf "%d %c %d = %d\n" first_number operator second_number (first_number / second_number)
    | '*' | 'x' | 'X' -> Printf.printf "%d %c %d = %d\n" first_number operator second_number (first_number * second_number)
    | _ -> Printf.printf "Sorry, you entererd a wrong operator '%c' so your calculation was not processed \n" operator

let calculate_floats operator first_number second_number =
  match operator with
    | '+' -> Printf.printf "%f %c %f = %f\n" first_number operator second_number (first_number +. second_number)
    | '-' -> Printf.printf "%f %c %f = %f\n" first_number operator second_number (first_number -. second_number)
    | '/' -> Printf.printf "%f %c %f = %f\n" first_number operator second_number (first_number /. second_number)
    | '*' | 'x' | 'X' -> Printf.printf "%f %c %f = %f\n" first_number operator second_number (first_number *. second_number)
    | _ -> Printf.printf "Sorry, you entererd a wrong operator '%c' so your calculation was not processed \n" operator

let () =
  print_string "Enter the first value: ";
  let first_number = read_line () in
  print_string "Enter the operator (+, -, /, (* or x or X)): ";
  let operator = (read_line ()).[0] in 
  print_string "Enter the second value: ";
  let second_number = read_line () in
  match (parse_number(first_number), parse_number(second_number)) with
    | (Int _, Int _) -> 
      calculate_integers operator (int_of_string first_number) (int_of_string second_number)
    | (Float _, _) | (_, Float _) -> 
      calculate_floats operator (float_of_string first_number) (float_of_string second_number)
