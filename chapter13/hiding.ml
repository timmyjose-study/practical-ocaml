(* "abstract" module *)

module type Restr = 
  sig
    val superB : int -> int
  end

(* implementations *)

module Test:Restr = 
  struct
    module B = 
      struct
        let b x = x * 10
      end
    let superB x = B.b x
  end

module AnotherTest:Restr = 
  struct
    module B = 
      struct
        let b x = x * 2
      end
    let superB x = B.b x
  end

