let myfunc (x: int) (y: int) =
  let someval = x + y in
  Printf.printf "Internal value = %i\n" someval

let errorprone (x: int ref) = 
  try
    while !x < 10 do
      incr x
    done
  with
    _ -> Printf.printf "%s\n" "An error occurred"

let mismatching (x: int) (y: float): int = 
  x + Int.of_float y

let mycompare (f: 'a -> 'a -> bool)  (x: 'a) (y: 'a): bool = f x y

(* curried functions *)

let add_one = ( + ) 1 ;;

let mul_two = ( * ) 2 ;;

(* HOFs *)

let sum = List.fold_left ( + ) 0

let product = List.fold_left ( * ) 1

(* function composition *)
let compose (f: 'a -> 'b) (g: 'b -> 'c): ('a -> 'c) = fun x -> g (f x)

let add_ten (x: int): int = x + 10

let mul_ten (x: int): int = x * 10

let add_then_mul : int -> int = compose add_ten mul_ten

let mul_then_add : int -> int = compose mul_ten add_ten

let sub_ten (x: int): int = x - 10

let add_then_mul_then_sub: int -> int = compose (compose add_ten mul_ten) sub_ten

type distance = 
  Meter of int
  | Foot of int
  | Mile of int

let to_meter (d: distance): distance = 
  match d with
  Meter n -> Meter n
  | Foot f -> Meter (f / 3)
  | Mile m -> Meter (m * 1600)

let to_foot (d: distance): distance = 
  match d with
  Meter n -> Foot (n * 3)
  | Foot f -> Foot f
  | Mile m -> Foot (m * 5000)

let to_mile (d: distance): distance = 
  match d with
  Meter n -> Mile (n / 1600)
  | Foot f -> Mile (f / 5000)
  | Mile m -> Mile m

let rec fib (n: int): int = 
  if n < 0 
  then raise (Invalid_argument "negative Fibonacci numbers are not defined")
  else
    match n with
    0 -> 0
  | 1 -> 1
  | 2 -> 1
  | _ -> fib (n - 1) + fib (n - 2)

(* conver a string into a list of chars *)
let explode_string (s: string): char list = 
  let len = String.length s in
    let rec explode_string_inner idx acc =
      if idx = len
      then List.rev acc
      else explode_string_inner (idx + 1) (s.[idx] :: acc)
  in
    explode_string_inner 0 []

(* collapse a list of chars into a string *)
let collapse_string (xs: char list): string =
  let buf = Buffer.create (List.length xs) in
    let rec collapse_string_innner l = 
      match l with
      [] -> Buffer.contents buf
      | h :: t -> Buffer.add_char buf h ; collapse_string_innner t
  in
    collapse_string_innner xs

let rec scan_input (scan_buf: Scanf.Scanning.scanbuf) (acc_buf: Buffer.t): string =
  try
    Scanf.bscanf scan_buf "%c" (fun c -> Buffer.add_char acc_buf c);
    scan_input scan_buf acc_buf
  with
    End_of_file -> Buffer.contents acc_buf

(* function parameter labels *)

let add_some_labelled  ~x ~y = x + y

let increment ?(by = 1) v = v + by

type person = {
  name: string;
  age: int;
  salary: float
}

let make_person ~name ~age ?(salary = 0.0) : person = 
  {
    name = name;
    age = age;
    salary = salary
}