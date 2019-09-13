let rec iter = fun (n,f) x->
  if n>0 then f (iter (n-1,f) x)
  else if n=0 then x
  else x(* impossible *)
  (*
let test = iter (0, fun x -> String.concat "," [x;"f"]) "d"
let test1 = iter (5, fun x -> x*2) 2
let _ = print_endline (test)
let _ = print_endline (string_of_int test1) *)