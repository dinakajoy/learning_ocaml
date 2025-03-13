open Yojson.Basic
open Yojson.Basic.Util

let json_file = "todos.json"

(* Read JSON file *)
let read_json filename =
  try Yojson.Basic.from_file filename
  with _ -> `List [] 

let write_json filename json =
  let oc = open_out filename in
  Printf.fprintf oc "%s\n" (Yojson.Basic.pretty_to_string json);
  close_out oc

let print_json filename =
  let json = read_json filename in
  print_endline (Yojson.Basic.pretty_to_string json)

let add_task json title description =
  match json with
  | `List lst -> `List (lst @ [`Assoc [("title", `String title); ("description", `String description); ("completed", `Boolean false)]])
  | _ -> failwith "Invalid JSON format"

let add_todo ?(description="") title = 
  let json = read_json json_file in
  let updated_json = add_task json title description in
  write_json json_file updated_json


  (* let todos = read_json "todos.json" in
  todos *)

(* let remove_todo () = "Removed"

let mark_completed () = "Completed" *)

let view_todos () = print_json json_file

let () =
  print_endline "To-Do List CLI App";
  print_endline "To add new task, enter 1: ";
  print_endline "To view all tasks, enter 2: ";
  print_endline "to view completed tasks, enter 3: ";
  print_endline "To view pending tasks, enter 4: ";
  print_endline "To mark a task as completed, enter 5: ";
  print_endline "To delete a task, enter 6: ";
  print_endline "To exit, enter 0: ";

  print_string "Enter your choice (1, 2, 3, 4, 5, 6, or 0): ";

  let choice = read_line () in
    match choice with 
    (* | "1" -> add_todo () *)
    | "1" -> view_todos ()
    | "2" -> view_todos ()
    | "3" -> view_todos ()
    | "4" -> view_todos ()
    | "5" -> view_todos ()
    | "6" -> view_todos ()
    | "0" -> view_todos ()
    | _ -> ()
