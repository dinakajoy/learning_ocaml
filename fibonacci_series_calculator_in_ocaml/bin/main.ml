(* Function to calculate the Fibonacci series up to n *)
let fibonacci n =
  if n <= 0 then [0]
  else if n = 1 then [1; 0]
  else if n = 2 then [1; 1; 0]
  else 
    let rec fib n b fib_count =
      if fib_count = n then List.rev b
      else match b with
      | first::second::_ -> 
        let sum = first + second in 
        let new_list = sum :: b in 
        let new_count = fib_count + 1 in 
        fib n new_list new_count
      | _ -> b
    in
    fib n [1; 1; 0] 2

let () =
  print_string "Enter the whole number of Fibonacci numbers to generate: ";
  let n = read_int () in
  let fib_list = fibonacci n in
  Printf.printf "Fibonacci series up to %d: " n;
  List.iter (Printf.printf "%d ") fib_list;
  print_endline ""