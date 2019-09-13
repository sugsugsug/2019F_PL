type crazy2 = NIL
              | ZERO of crazy2
              | ONE of crazy2
              | MONE of crazy2

let rec crazy2val = fun x ->
  match x with
  |NIL -> 0
  |ZERO c -> 2 * crazy2val(c)
  |ONE c -> 2 * crazy2val(c) + 1
  |MONE c -> 2 * crazy2val(c) - 1
(*
let test = crazy2val(ZERO(ONE(MONE NIL)))
let _ = print_endline (string_of_int test)
        *)