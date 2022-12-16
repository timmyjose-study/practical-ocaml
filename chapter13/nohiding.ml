(* contrast this with hiding.ml *)

module Test = 
  struct
    module B = 
      struct
        let b x = x * 3 (* not hidden anymore *)
      end
    let superB x = B.b x
  end