type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
          | Poland | Portugal | Italy | Germany | Norway | Sweden | England
          | Argentina
type tourna = LEAF of team
            | NODE of tourna * tourna


let rec parenize = fun x ->
  match x with
  |LEAF i -> 
    (match i with
    |Korea -> "Korea"
    |France -> "France"
    |Usa -> "Usa"
    |Brazil -> "Brazil"
    |Japan -> "Japan"
    |Nigeria -> "Nigeria"
    |Cameroon -> "Cameroon"
    |Poland -> "Poland"
    |Portugal -> "Portugal"
    |Italy -> "Italy"
    |Germany -> "Germany"
    |Norway -> "Norway"
    |Sweden -> "Sweden"
    |England -> "England"
    |Argentina -> "Argentina"
    )
  |NODE (t1,t2) -> String.concat "" ["(";
    (String.concat " " [parenize(t1);parenize(t2)]);")"]
(*
let test = parenize(NODE(NODE(LEAF Korea, LEAF Portugal), LEAF Brazil))
let _ = print_endline (test) *)