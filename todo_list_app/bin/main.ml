let json_file = "todos.json"

(* Read JSON file *)
let read_json filename =
  try Yojson.Basic.from_file filename
  with _ -> `List [] 

(* Write to JSON file *)
let write_json filename json =
  let oc = open_out filename in
  Printf.fprintf oc "%s\n" (Yojson.Basic.pretty_to_string json);
  close_out oc

let get_json_file_length filename =
  let json = read_json filename in
  match json with
  | `List lst -> List.length lst
  | _ -> 0

let add_task filename task =
  let json = read_json filename in
  match json with
  | `List lst ->
      let updated_list = `List (lst @ [task]) in
      write_json filename updated_list;
      print_endline "Task added."
  | _ -> print_endline "Invalid JSON format: Expected a list."

let print_tasks filename =
  let json = read_json filename in
  print_endline (Yojson.Basic.pretty_to_string json)

let print_task_by_id filename id =
  let json = read_json filename in
  match json with
  | `List lst ->
      let item = List.find_opt (function
        | `Assoc assoc -> List.assoc_opt "id" assoc = Some (`String id)
        | _ -> false
      ) lst in
      (match item with
      | Some i -> print_endline (Yojson.Basic.pretty_to_string i)
      | None -> print_endline ("Item with ID " ^ id ^ " not found."))
  | _ -> print_endline "Invalid JSON format"

let print_tasks_by_status filename status =
  let json = read_json filename in
  match json with
  | `List lst ->
      let filtered_tasks = List.filter (function
        | `Assoc assoc -> (match status with
          | "completed" -> List.assoc_opt "completed" assoc = Some (`Bool true)
          | "pending" -> List.assoc_opt "completed" assoc = Some (`Bool false)
          | _ -> false)
        | _ -> false
      ) lst in
      if filtered_tasks = [] then
        print_endline (match status with
          | "completed" -> "No completed tasks found."
          | "pending" -> "No pending tasks found."
          | _ -> "No tasks found.")
      else
        print_endline (Yojson.Basic.pretty_to_string (`List filtered_tasks))
  | _ -> print_endline "Invalid JSON format"

let mark_task_completed filename id =
  let json = read_json filename in
  match json with
  | `List lst ->
      let updated_list = List.map (function
        | `Assoc assoc as task ->
            (match List.assoc_opt "id" assoc with
            | Some (`String task_id) when task_id = id ->
                `Assoc (List.map (fun (k, v) ->
                  if k = "completed" then (k, `Bool true) else (k, v)
                ) assoc)
            | _ -> task)
        | other -> other
      ) lst in
      let updated_json = `List updated_list in
      write_json filename updated_json;
      print_endline ("Task with ID " ^ id ^ " marked as completed.")
  | _ -> print_endline "Invalid JSON format"

(* let remove_todo () = "Removed"m*)
let delete_task filename id =
  let json = read_json filename in
  match json with
  | `List lst ->
      let updated_list = List.filter (function
        | `Assoc assoc -> (match List.assoc_opt "id" assoc with
          | Some (`String task_id) when task_id <> id -> true
          | _ -> false)
        | _ -> true
      ) lst in
      let updated_json = `List updated_list in
      write_json filename updated_json;
      print_endline ("Task with ID " ^ id ^ " deleted.")
  | _ -> print_endline "Invalid JSON format"

let add_todo ?(description="") title =
  let id = string_of_int (get_json_file_length json_file + 1) in
  let new_task = `Assoc [
    ("id", `String id);
    ("title", `String title);
    ("description", `String description);
    ("completed", `Bool false)
  ] in
  add_task json_file new_task

let get_todo_details () =
  print_string "Enter your title: ";
  let title = read_line () in
  print_string "Enter your description: ";
  let description = read_line () in
  add_todo ~description title

let view_todos () = print_tasks json_file

let view_todo () =
  print_string "Enter your id: ";
  let id = read_line () in 
  print_task_by_id json_file id

let view_completed_todos () = print_tasks_by_status json_file "completed"

let view_pending_todos () = print_tasks_by_status json_file "pending"

let mark_completed () = 
  print_string "Enter your id: ";
  let id = read_line () in 
  mark_task_completed json_file id

let delete_todo () =
  print_string "Enter your id: ";
  let id = read_line () in 
  delete_task json_file id

let () =
  print_endline "To-Do List CLI App";
  print_endline "To add new task, enter 1: ";
  print_endline "To view all tasks, enter 2: ";
  print_endline "To view a task by id, enter 3: ";
  print_endline "To view completed tasks, enter 4: ";
  print_endline "To view pending tasks, enter 5: ";
  print_endline "To mark a task as completed, enter 6: ";
  print_endline "To delete a task, enter 7: ";
  print_endline "To exit, enter 0: ";

  print_string "Enter your choice (1, 2, 3, 4, 5, 6, 7, or 0): ";

  let choice = read_line () in
    match choice with 
    | "1" -> get_todo_details ()
    | "2" -> view_todos ()
    | "3" -> view_todo ()
    | "4" -> view_completed_todos ()
    | "5" -> view_pending_todos ()
    | "6" -> mark_completed ()
    | "7" -> delete_todo ()
    | "0" -> 
      print_endline "Exiting the application. Goodbye!";
      exit 0
    | _ -> print_endline "Invalid choice. Please try again."
