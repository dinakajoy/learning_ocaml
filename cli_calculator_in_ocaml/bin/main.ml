let () =
  print_string "Enter the first value: ";
  let first_number = read_int () in
  print_string "Enter the operator (+, -, /, * or x or X): ";
  let operator = (read_line ()).[0] in 
  print_string "Enter the second value: ";
  let second_number = read_int () in 

  match operator with
  | '+' -> Printf.printf "%d %c %d = %d\n" first_number operator second_number (first_number + second_number)
  | '-' -> Printf.printf "%d %c %d = %d\n" first_number operator second_number (first_number - second_number)
  | '/' -> Printf.printf "%d %c %d = %d\n" first_number operator second_number (first_number / second_number)
  | '*' | 'x' | 'X' -> Printf.printf "%d %c %d = %d\n" first_number operator second_number (first_number * second_number)
  | _ -> Printf.printf "Sorry, you entererd a wrong operator '%c' so your calculation was not processed \n" operator
