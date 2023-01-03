(*CSCI 2041 Lab10 Jiatan Huang && Ziyue Zhuang*)
open List;;
type 
  thing = 
    Cons of thing * thing |
    Nil |
    Number of int |
    Symbol of string;;

let rec every func thing = 
  match thing with
  Cons(left, right) -> 
    if (func left == false)
      then false
  else
    (every func right)|
    _ -> true;; 


let rec substitute thing oldele newele = 
  match thing with
  Cons(left, right) -> 
    if left = oldele
      then Cons(newele, substitute right oldele newele)
  else
    Cons(left, substitute right oldele newele)|
    _ -> Nil;;


let rec questyEqual left right = 
  match (left,right) with
  (Nil,Nil) -> true|
  (Symbol "?",_)->true|
  (Number left, Number right)->
    if left = right
      then true
  else
    false|
  (Symbol left, Symbol right)->
    if left = right
      then true
  else
    false|
  (Cons(lleft, lright),Cons(rleft, rright))-> 
    if (questyEqual lleft rleft)
      then (questyEqual lright rright)
    else
      false|
  (_,_) -> false;;


(*test result*)
(*type thing = Cons of thing * thing | Nil | Number of int | Symbol of string
val every : (thing -> bool) -> thing -> bool = <fun>
val substitute : thing -> thing -> thing -> thing = <fun>
val questyEqual : thing -> thing -> bool = <fun>*)
(*val isCons : thing -> bool = <fun>
val isZero : thing -> bool = <fun>
- : bool = true
- : bool = true
- : bool = true
- : bool = false
- : bool = true
- : bool = true
- : bool = false
- : thing = Nil
- : thing = Cons (Symbol "B", Cons (Symbol "B", Cons (Symbol "B", Nil)))
- : thing = Cons (Symbol "X", Cons (Symbol "Y", Cons (Symbol "Z", Nil)))
- : thing = Cons (Symbol "A", Cons (Cons (Symbol "B", Nil), Nil))
- : bool = true
- : bool = false
- : bool = true
- : bool = true
- : bool = false
- : bool = true
- : bool = true
- : bool = true*)
