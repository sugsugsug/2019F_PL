type expr = NUM of int
              | PLUS of expr * expr
              | MINUS of expr * expr
              | MULT of expr * expr
              | DIVIDE of expr * expr
              | MAX of expr list

let rec eval = fun x ->
  match x with
  |NUM y -> y
  |PLUS (e1,e2) -> eval e1 + eval e2
  |MINUS (e1,e2) -> eval e1 - eval e2
  |MULT (e1,e2) -> eval e1 * eval e2
  |DIVIDE (e1,e2) -> eval e1 / eval e2
  |MAX l ->
    (match l with
    | [] -> 0
    | head::tail -> if List.length tail=0 then eval (head)
                    else (if eval (head) > eval (MAX(tail))
                          then eval (head)
                          else eval (MAX(tail)) ) )

(*
let rec showcrazy2 = fun x ->
  match x with
  |NIL -> ""
  |ZERO d -> String.concat "" ["0";showcrazy2(d)]
  |ONE d -> String.concat "" ["+";showcrazy2(d)]
  |MONE d -> String.concat "" ["-";showcrazy2(d)]

let t1 = eval (MAX [NUM 1; NUM 1; NUM 2; NUM 1; NUM 1])
let t2 = eval (MAX [NUM (-98765)])
let test = eval(DIVIDE (NUM (-3000), NUM 22))
let _ = print_endline (string_of_int t1)
let _ = print_endline (string_of_int t2)
let _ = print_endline (string_of_int test) *)