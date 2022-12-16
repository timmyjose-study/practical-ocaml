let trycatch_demo () = 
 try
   Printf.printf "%i\n" (100 / 2);
  with
    _ -> Printf.printf "error 0\n ";

  try
    Printf.printf "%i\n" (10 / 0);
  with
  _ -> Printf.printf "error 1\n";

  try
    Printf.printf "%i\n" (10 / 2);
  with
    _ -> Printf.printf "error 2\n ";
