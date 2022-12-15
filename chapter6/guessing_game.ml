open Random;;
Random.self_init ();;

let get_random_number low high = 
  (Random.int (high - low)) + low

let rec play secret = 
  Printf.printf "What is your guess?\n";
  let guess = read_int () in
    if guess < secret
    then (Printf.printf "Too low!\n"; play secret)
    else if guess > secret
          then (Printf.printf "Too high!\n"; play secret)
          else Printf.printf "Correct! You win!\n"

let main = 
  let secret = get_random_number 1 100 in
    play secret

