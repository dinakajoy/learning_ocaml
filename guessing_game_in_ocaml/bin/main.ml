(* Generates a random secret number between 1 and 100 *)
let () = Random.self_init ()
let secret_number = Random.int 100 + 1

(* Function to play the guessing game *)
let rec guess_number remaining_attempts =
  if remaining_attempts = 0 then
    print_endline "Sorry, you have used all your attempts. Game over!"
  else begin
    print_string "Guess the secret number (between 1 and 100): ";
    match read_int () with
    | guess when guess < 1 || guess > 100 ->
        print_endline "Please enter a number between 1 and 100.";
        guess_number remaining_attempts
    | guess when guess = secret_number ->
        Printf.printf "Congratulations! You've guessed the secret number: %d\n" secret_number
    | guess when guess < secret_number ->
        print_endline "Too low! Try again.";
        guess_number (remaining_attempts - 1)
    | guess when guess > secret_number ->
        print_endline "Too high! Try again.";
        guess_number (remaining_attempts - 1)
    | _ -> ()
  end

(* Start the game with 5 attempts *)
let () = guess_number 5