let cracklepop num =
  let num_is_divisible_by_3 = num mod 3 = 0 in
  let num_is_divisible_by_5 = num mod 5 = 0 in

  match (num_is_divisible_by_3, num_is_divisible_by_5) with
  | (true, true) -> "CracklePop"
  | (true, false) -> "Crackle"
  | (false, true) -> "Pop"
  | (false, false) -> string_of_int num

let rec print_cracklepop_sequence start_num end_num =
  if start_num <= end_num then begin
    print_endline (cracklepop start_num);
    print_cracklepop_sequence (start_num + 1) end_num
  end

let () =
  print_cracklepop_sequence 1 100