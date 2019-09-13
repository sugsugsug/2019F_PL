type crazy2 = NIL
              | ZERO of crazy2
              | ONE of crazy2
              | MONE of crazy2

let rec crazy2add = fun (x,y) ->
  match x with
  |NIL -> y
  |ZERO c ->
    (match y with
    |NIL -> ZERO(c)
    |ZERO d -> ZERO(crazy2add(c,d))
    |ONE d -> ONE(crazy2add(c,d))
    |MONE d -> MONE(crazy2add(c,d)))
  |ONE c ->
    (match y with
    |NIL -> ONE(c)
    |ZERO d -> ONE(crazy2add(c,d))
    |ONE d -> ZERO(crazy2add(crazy2add(c,d),ONE(NIL)))
    |MONE d -> ZERO(crazy2add(c,d)))
  |MONE c ->
    (match y with
    |NIL -> MONE(c)
    |ZERO d -> MONE(crazy2add(c,d))
    |MONE d -> ZERO(crazy2add(crazy2add(c,d),MONE(NIL)))
    |ONE d -> ZERO(crazy2add(c,d)))
(*
let rec showcrazy2 = fun x ->
  match x with
  |NIL -> ""
  |ZERO d -> String.concat "" ["0";showcrazy2(d)]
  |ONE d -> String.concat "" ["+";showcrazy2(d)]
  |MONE d -> String.concat "" ["-";showcrazy2(d)]

let t1 = ONE(MONE NIL)
let t2 = ONE(MONE(MONE NIL))
let test = crazy2add(t1,t2)
let _ = print_endline (showcrazy2 t1)
let _ = print_endline (showcrazy2 t2)
let _ = print_endline (showcrazy2 test) *)