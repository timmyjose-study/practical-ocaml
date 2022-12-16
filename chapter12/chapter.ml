(*
basic usage:
  $ ocamldoc -html chapter.ml
*)

(** a factorial function *)
let rec factorial (n: int): int = 
  if n <= 0
  then 1
  else n * factorial (n - 1)