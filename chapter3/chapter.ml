(* record types *)
type person = { name: string; age: int; salary: float }

let bob: person = { name = "Bob"; age =  42; salary = 123.45 }

(* enumeration types *)

type daysoftheweek = 
  Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursdau
  | Friday
  | Saturday

let dow: daysoftheweek = Sunday

(* aggregate types *)

type ('a, 'b) tree = 
  Leaf of 'a
  | Node of (('a, 'b) tree * 'b * ('a, 'b) tree)

let tree: (int, string) tree = Node (Leaf 100, "mytree", Leaf 200)

type 'a polyfish = Polyfish of 'a

type fish = Fish of int

let print_polyfish f = 
  match f with
  Polyfish fish -> Printf.printf "%s\n" fish

type 'a distance =
  Metre of int  
  | Foot of int 
  | Mile of int
  | Distance of 'a distance

type 'a part_with_length = 
  {
    part_name: string;
    part_number: int;
    part_length: 'a distance
}

let crescent_wrench: 'a part_with_length = {
  part_name = "Left-handed crescent wrench";
  part_number = 1;
  part_length = Foot 1
  }

(* mutability in records *)

type 'a mutable_part_with_length = {
  mutable mpart_name: string;
  mpart_number: int;
  mpart_length: 'a distance
}

let mcrescent_wrench = {
  mpart_name = "Right-handed crescent wrench";
  mpart_number = 1;
  mpart_length = Metre 1
}

type fraction = { 
  numerator: int;
  denominator: int
}

let add (n: int): int = 
  n + 1

let rec euclid (a: int) (b: int): int = 
  match b with
  0 -> a
  | _ -> euclid b (a mod b)

let gcd (a: int) (b: int): int = 
  let n = ref a in
    let m = ref b in
      while !m <> 0 do
        let t = !n mod !m in
          n := !m;
          m := t
      done;
      !n

type distance' = Metre of float | Foot of float

let convert (d: distance'): distance' = 
  match d with
  Foot f -> Metre (f *. 0.3048)
  | Metre m -> Foot (m /. 0.3048)