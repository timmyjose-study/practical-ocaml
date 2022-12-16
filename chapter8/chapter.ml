(* Collections *)

type university_person = 
  Grad_student
  | Undergrad_student
  | At_large_student
  | Adjunct_professor
  | Professor
  | Staff

let assortment = [ Grad_student; Staff; Adjunct_professor; Undergrad_student ]

let rank uni =
  match uni with
  Grad_student -> 0
  | Undergrad_student -> 1
  | At_large_student -> 2
  | Adjunct_professor -> 4
  | Professor -> 5
  | Staff -> 3

(* custom comparator *)
let university_comparator x y = compare (rank x) (rank y)

(* List *)

let example_list = [10; 20; 30; 40; 50; 60; 70; 80; 90]

let phone_book = [("Bob", 123); ("Anne", 991); ("Dave", 827); ("Hannah", 643); ("Larry", 980)]

(* Arrays and Matrices *)

open Random;;
Random.self_init ();;

let random_array = Array.init 10 (fun x -> x + Random.int 100)

let simple_array = Array.make 10 100L

(* Hash tables - note that duplicate keys are allowed! *)

let hashtbl_demo () = 
  let process_find d k = 
    try
      let v = Hashtbl.find d k in
        Printf.printf "Key %i found with value %s\n" k v
    with
      Not_found -> Printf.printf "Key %i not found\n" k
  in
    let mydict = Hashtbl.create 1 in
      Printf.printf "Initial hash table created with size %i\n" (Hashtbl.length mydict);

      Hashtbl.add mydict 1 "hola";
      Hashtbl.add mydict 2 "mundo";
      Hashtbl.add mydict 3 "como";
      Hashtbl.add mydict 4 "estas?";
      Hashtbl.add mydict 5 "adios!";

      Printf.printf "New hash table size = %i\n" (Hashtbl.length mydict);
      Printf.printf "Hash table contents...\n";
      Hashtbl.iter (fun k v -> Printf.printf "%i => %s\n" k v) mydict;

      process_find mydict 100;
      process_find mydict 5;

      Hashtbl.clear mydict;
      Printf.printf "After clearing, length of hash table = %i\n" (Hashtbl.length mydict);

(* Sets - purely functional Functors *)

module StrSet = Set.Make(String)

let set_demo () = 
  let myset = MySet.add "hello" MySet.empty in
    let myset = MySet.add "world" myset in 
      StrSet.iter (fun e -> Printf.printf "%s\n" e) myset;

