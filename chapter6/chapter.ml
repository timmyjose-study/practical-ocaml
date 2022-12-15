(* l -> int32, L -> int65, n -> nativeint *)
let rec fibonacci (n: int64): int64 = 
  if n < 0L
  then 1L
  else 
    match n with
    0L -> 0L
    | 1L -> 1L
    | 2L -> 1L
    | _ -> Int64.add (fibonacci (Int64.sub n 1L)) (fibonacci (Int64.sub n 2L))

(* Range pattern matching *)

let whatthefunc c = 
  match c with
  'a' .. 'c' -> Printf.printf "Pass\n"
    | 'd' -> Printf.printf "Problematic\n"
    | 'e' | 'f' -> Printf.printf "Terrible\n"
    | _ -> Printf.printf "Adios!\n"

let raise_if_unequal x y e = 
  if x <> y 
  then raise e

(* lazy evaluation *)
(*
  Call the function below as:

    # somef (1 / 0) ;;
  and
    # somef (lazy (1 / 0))

  to see the difference.

  To create a lazy value:

    # let b = lazy (10 + 30)

  To use it:

    # Lazy.force b

  and from the on, `b` will return the evaliated value (force is idempotent).
*)

let somef x = 100

(* polymorphic types with multiple type variables *)

type ('a, 'b, 'c) polytype = 
  ConstOne of 'a
    | ConstTwo of 'a * 'b
    | ConstThree of 'a * ('b * 'c)