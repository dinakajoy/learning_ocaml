
let read_json filename =
  let json = Yojson.Basic.from_file filename in
  print_endline (Yojson.Basic.pretty_to_string json)

(* let add_todo () = 
  let todos = read_json "todos.json" in
  todos

let remove_todo () = "Removed"

let mark_completed () = "Completed"

let list_todos () = "All Todos" *)


let () = 
  let todos = read_json "todos.json" in
  todos