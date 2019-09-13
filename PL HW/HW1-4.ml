type formula = TRUE 
              | FALSE
              | NOT of formula
              | ANDALSO of formula * formula
              | ORELSE of formula * formula
              | IMPLY of formula * formula
              | LESS of expr * expr
    and expr = NUM of int 
              | PLUS of expr * expr
              | MINUS of expr * expr

let rec eval_expr = fun x ->
  match x with
  |NUM n -> n
  |PLUS (e1,e2) -> eval_expr(e1) + eval_expr(e2)
  |MINUS (e1,e2) -> eval_expr(e1) - eval_expr(e2)
let rec eval = fun x ->
  match x with
  |TRUE -> true
  |FALSE -> false
  |NOT f -> 
  (match eval(f) with
    | true -> false
    | false -> true)
  |ANDALSO (f1,f2) -> (eval(f1) && eval(f2))
  |ORELSE (f1,f2) -> (eval(f1) || eval(f2))
  |IMPLY (f1,f2) -> (if eval(f1) && eval(f2)=false 
                    then false else true)
  |LESS (e1,e2) -> eval_expr(e1) < eval_expr(e2)
(*
let test = eval(LESS (PLUS (NUM 10, NUM 12), MINUS (NUM 10, NUM (-13))))
let test2 = eval(ORELSE (IMPLY(LESS (NUM (-10), NUM (-100)), ANDALSO (NOT TRUE, TRUE)), ANDALSO (TRUE, ANDALSO (LESS (NUM 10, PLUS (MINUS (NUM 10, NUM (-10)), NUM 30)), TRUE))))
let test3 = eval(LESS (PLUS (NUM 3, NUM 5), PLUS (NUM 1, NUM 2)))
let _ = if test3 then print_endline ("TRUE")
        else print_endline ("FALSE")
        *)