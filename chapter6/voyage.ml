open Random;;
Random.self_init ();;

let situations = [| "Ship about to explode"; "Ship hailing us"; "Klingons off the Starboard bow"|] 
let responses = [| "Hail ship"; "Send friendship message"; "Shoot to kill"; "Abandon ship!"|]

let display_current_situation () = 
  Printf.printf "Captain %s, what do we do?\n"
  (Array.get situations (Random.int (Array.length situations)))

let show_menu lst = 
  Array.iteri (fun x y -> Printf.printf "%i      %s\n" x y) lst;
  Printf.printf "\nResponse? "

let respond x = 
  match x with
  "Hail ship" -> "Hailing, sir!"
  | "Send friendship message" -> "They like me, they really like me!"
  | "Shoot to kill" -> "But, we come in peace!"
  | "Abandon ship!" -> "Iceberg, right ahead!"
  |_ -> "Captain, I just don't understand you!"

let _ = 
  display_current_situation();
  show_menu responses;
  Printf.printf "%s\n"
    (respond (Array.get responses (int_of_string (read_line ()))))