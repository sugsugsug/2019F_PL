let rec sigma = fun (a,b,f) ->
  if b>a then (f b) + (sigma (a,b-1,f))
  else if b=a then (f a)
  else 0   (* impossible *)
  
(*
let test = sigma (1,4,(fun n -> n+1))

let _ = print_endline (string_of_int test)
*)