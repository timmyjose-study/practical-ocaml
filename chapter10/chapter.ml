let raise_demo () = 
  try
    raise Not_found
  with
    Not_found -> ()

let failure_demo () = 
  try
    failwith "Oh, noes!"
  with
    Failure s -> Printf.printf "%s\n" s

let exception_demo () = 
  try
    raise Not_found
  with
    Failure s -> Printf.printf "%s\n" s
    | _ -> Printf.printf "Something\n"

(* custom exceptions *)

exception MyException of string

open Random;;
Random.self_init ();;

let my_exception_demo () = 
  let inner () = 
    let rand = Random.int 10 in
      match rand mod 2 with
      0 -> raise (MyException "uh-oh! an even!")
    | _ -> Printf.printf "I can live with that - %i!\n" rand
  in
    for _ = 1 to 10 do
      try inner ()
      with 
        MyException msg -> Printf.fprintf stderr "%s\n" msg;
    done

