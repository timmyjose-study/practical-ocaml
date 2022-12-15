let rec factorial (n: int): int = 
  if n <= 0 
  then 1
  else n * factorial (n - 1)

let _ = Printf.printf "Factorial of %i = %i\n" 20 (factorial 20)